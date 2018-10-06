#' @importFrom Rdpack reprompt
#' @import spacetime
#' @import lubridate
#' @import trajectories
#' @import rgdal
#' @import sp
#' @import rgeos
NULL

#' Identifies and classifies visits in GPS tracks.
#'
#' \code{locationsTrack} identifies clusters (locations) of
#' points in GPS tracks (\code{\link[trajectories]{Track}}
#' object) based on their spatial proximity using
#' \code{\link{extractClustersBuffer}} and identifies individual
#' visits of the same cluster (location) along the track.
#' Additionally, visits are classified as long-term visits
#' (campsites) or short-term visits.
#'
#' The function can be used in order to assign to each data
#' value of the input \code{\link[trajectories]{Track}}
#' object an id of the cluster it is assigned to (
#' \code{summary = FALSE}) or to summarise the information
#' for each visit of a location (\code{summary = TRUE}).
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object.
#' @param radius A numerical value representing the radius of the
#' buffers computed around each point [m] which are used for
#' clustering values to locations. Default is \code{radius = 800} [m].
#' @param tmin The minimum residence time at a specific location [s]
#' which is used to classify visits as campsites (if the residence
#' time at a specific location is larger than \code{tmin}) in
#' contrast to short-term visits of locations. The default is
#' \code{tmin = 345600}, i.e. 4 days.
#' @param tmaxinterstices The maximum time between subsequent visits
#' at the same location in case of which the duration of these visits
#' will be added in order to classify both visits together as long-term
#' visit (campsite) or short-term visit, based on \code{tmin}.
#' @param timeinterval A numerical value reperesenting the duration
#' of a time interval represented by one data value of
#' \code{currenttrack} [s].
#' @param summary Logical value indicating if the information on the
#' locations and visits should be summarised (\code{summary = TRUE})
#' or not (\code{summary = FALSE}). See the details section for further
#' information.
#' @return
#' \describe{
#'   \item{If (\code{summary = FALSE})}{A
#'   \code{\link[trajectories]{Track}} object that is identical
#'   to the input \code{\link[trajectories]{Track}} object, but
#'   has four additional columns in the \code{data} slot:
#'   \describe{
#'     \item {\code{location}}{An integer value for each identified
#'     spatial point cluster (location) increasing with the time starting
#'     from 1.}
#'     \item{\code{campsite}}{A logical value indicating if a visits of a
#'     location is classified as long-term visit (campsite) (\code{TRUE})
#'     or as short-term visit (\code{FALSE}).}
#'     \item{\code{visitsloc}}{An integer vector indicating the number of
#'     visits at a specific location the data point is assigned to (i.e.
#'     a counter for the visits at a specific location).}
#'     \item{\code{visitscampsite}}{An integer vector indicating the number
#'     of visits at a specific location the data point is assigned to that
#'     are classified as long-term visits (campsites) (i.e.
#'     a counter for the long-term visits at a specific location).}
#'   }
#'   Gaps, as indicated by the column \code{gap}, have \code{NA} values for
#'   all four variables.
#'   }
#'   \item{If (\code{summary = TRUE})}{A \code{data.frame}
#'   object summarising the locations and visits of the input
#'   \code{\link[trajectories]{Track}} object with the following variables:
#'   \describe{
#'     \item{location}{An integer value for each identified
#'     spatial point cluster (location) increasing with the time starting
#'     from 1.}
#'     \code{lon}{The longitude of the respective location (as mean value of
#'     the coordinates of the data values assigned to the visit).}
#'     \code{lat}{The latitude of the respective location (as mean value of
#'     the coordinates of the data values assigned to the visit).}
#'     \code{alt}{The altitude of the respective location (as mean value of
#'     the coordinates of the data values assigned to the visit).}
#'     \item{\code{visitsloc}}{An integer vector indicating the number of
#'     visits at a specific location the data point is assigned to (i.e.
#'     a counter for the visits at a specific location).}
#'     \item{campsite}{A logical value indicating if a visits of a
#'     location is classified as long-term visit (campsite) (\code{TRUE})
#'     or as short-term visit (\code{FALSE}).}
#'     \item{\code{visitscampsite}}{An integer vector indicating the number
#'     of visits at a specific location the data point is assigned to that
#'     are classified as long-term visits (campsites) (i.e.
#'     a counter for the long-term visits at a specific location).}
#'     \item{arrivaltime}{A \code{POSIXct} vector indicating the arrival time
#'     of the visit at the respective location.}
#'     \item{departuretime}{A \code{POSIXct} vector indicating the departure
#'     time of the visit from the respective location.}
#'     \item{residencetime}{A numerical vector indicating the residence time
#'     of each visit [s].}
#'     \code{speed}{The speed of the respective location (as mean value of
#'     the speed values of the data values assigned to the visit).}
#'   }
#'   }
#' }
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{redefineIndices}},
#' \code{\link{fillGapTrack}}, \code{\link{fillGapTracks}},
#' \code{\link{extractClustersBuffer}}.
#' @examples #
#' @export
locationsTrack <- function(currenttrack,
                           radius = 200,
                           tmin = 345600,
                           tmaxinterstices = 345600,
                           timeinterval = 30*60,
                           summary = TRUE){

  # convert track to SpatialPointsDataFrame and transform to UTM
  trsSP <- TrackToSpatialPointsDataFrame(currenttrack)

  # cluster the data points
  trsSP$location <- extractClustersBuffer(trsSP, radius)

  # return NULL if a data set contains only one location (no Track object can be constructed from one point)
  if(length(unique(trsSP$location)) == 1){
    warning("Only one location identified!")
    return(NULL)
  }

  # redefine indices of locations (according to arrival time)
  trsSP@data <- redefineIndices(df = trsSP@data, indices = "location", time = "time", notchange = 0)

  # extract the data value indices of visits at each locations
  trackindicesvisits <- do.call(rbind, lapply(unique(trsSP$location), function(x){

    # get the block start and end indices for the current location
    blockindices <- identifyBlocksVariable(currenttrack = trsSP, variable = "location", value = x)

    row.names(blockindices) <- rep(x, nrow(blockindices))
    return(blockindices)

  }))

  # extract the location information
  locations <- as.numeric(row.names(trackindicesvisits))

  # count the number of repeated visits at each location
  trackindicesvisits <- countAllReapeatedVisits(trackindicesvisits, locations)

  # aggregate repeated visits at the same location if the interstice is < tmaxinterstices
  trackindicesvisits <- aggregateRepeatedVisits(trackindicesvisits, locations, intersticesduration, timeinterval)

  # compute the duration of the aggregated visits
  trackindicesvisits <- do.call(rbind, lapply(seq_len(nrow(trackindicesvisits)), function(x){

    # define an index for the same location and aggregatedvisit
    index <- which(row.names(trackindicesvisits) == row.names(trackindicesvisits)[x] & trackindicesvisits[,4] == trackindicesvisits[x,4])
    c(trackindicesvisits[x,], sum(trackindicesvisits[index,2] - trackindicesvisits[index,1])+1)

  }))

  # classify visits as long-term visit (campsite) or short-term visit
  trackindicesvisits <- classifyVisits(trackindicesvisits, tmin, timeinterval)

  # count the number of repeated long-term visits at each location
  trackindicesvisits <- countAllReapeatedLongTermVisits(trackindicesvisits, locations)

  # add locations to trackindicesvisits
  trackindicesvisits <- cbind(trackindicesvisits, locations)

  # order the entries of trackindicesvisits
  trackindicesvisits <- trackindicesvisits[order(trackindicesvisits[,1]),]

  # add all values to the input track object
  tracklocationsvisits <- do.call(rbind, lapply(seq_len(nrow(trackindicesvisits)), function(x){

    do.call(rbind, lapply(trackindicesvisits[x, 1]:trackindicesvisits[x, 2], function(y){
      trackindicesvisits[x, c(8, 6, 3, 7)]
    }))

  }))
  currenttrack$location <- tracklocationsvisits[,1]
  currenttrack$campsite <- tracklocationsvisits[,2]
  currenttrack$visitsloc <- tracklocationsvisits[,3]
  currenttrack$visitscampsite <- tracklocationsvisits[,4]

  # set the values for location, campsite, visitsloc and visitscampsite to NA for gaps
  currenttrack@data[which(currenttrack$location == 0),(ncol(currenttrack@data)-3):ncol(currenttrack@data)] <- NA

  # summarise the values for each visit and location
  if(summary == TRUE){

    # define an index for each location and visit
    indexvisitlocation <- paste0(currenttrack$location, "_", currenttrack$visitsloc)

    # compute mean coordinates for each visit
    coordinatesvisitlocation <- SpatialPoints(t(sapply(unique(indexvisitlocation), function(x) c(mean(trsSP@coords[indexvisitlocation == x,1]), mean(trsSP@coords[indexvisitlocation == x,2])))), proj4string = trsSP@proj4string)
    coordinatesvisitlocation <- spTransform(coordinatesvisitlocation, currenttrack@sp@proj4string)@coords

    summaryvisitslocation <- data.frame(
      location = sapply(unique(indexvisitlocation), function(x) currenttrack$location[indexvisitlocation == x][1]),
      lon = coordinatesvisitlocation[,1],
      lat = coordinatesvisitlocation[,2],
      alt = sapply(unique(indexvisitlocation), function(x) mean(as.numeric(currenttrack$HEIGHT[indexvisitlocation == x]))),
      visitsloc = sapply(unique(indexvisitlocation), function(x) currenttrack$visitsloc[indexvisitlocation == x][1]),
      campsite = sapply(unique(indexvisitlocation), function(x) currenttrack$campsite[indexvisitlocation == x][1]),
      visitscampsite = sapply(unique(indexvisitlocation), function(x) currenttrack$visitscampsite[indexvisitlocation == x][1]),
      arrivaltime = as.POSIXct(sapply(unique(indexvisitlocation), function(x) currenttrack$time[indexvisitlocation == x][1]), origin = "1970-01-01 00:00:00"),
      departuretime = as.POSIXct(sapply(unique(indexvisitlocation), function(x) currenttrack$time[indexvisitlocation == x][length(which(indexvisitlocation == x))]), origin = "1970-01-01 00:00:00"),
      residencetime = sapply(unique(indexvisitlocation), function(x) difftime(currenttrack$time[indexvisitlocation == x][length(which(indexvisitlocation == x))], currenttrack$time[indexvisitlocation == x][1], units = "sec")),
      speed = sapply(unique(indexvisitlocation), function(x) mean(as.numeric(currenttrack$SPEED[indexvisitlocation == x])))
        )

    summaryvisitslocation[-is.na(summaryvisitslocation$location),]

  }else{
    return(currenttrack)
  }

}

# function in order to count the number of repeated visits at each location
countAllReapeatedVisits <- function(trackindicesvisits, locations){

  cbind(trackindicesvisits, do.call(c, lapply(unique(locations), function(x){

    seq_along(which(locations == x))

  })))

}

# function in order to aggregate repeated visits at the same location if the interstice is < tmaxinterstices
aggregateRepeatedVisits <- function(trackindicesvisits, locations, intersticesduration, timeinterval){

  do.call(rbind, lapply(unique(locations), function(x){

    # get entries of trackindicesvisits of location x
    index <- which(locations == x)

    if(length(index) == 1){
      matrix(c(trackindicesvisits[index,], 1), nrow = 1, dimnames = list(x, NULL))
    }else{

      # get the duration of interstices (add a 0 for the first visit)
      intersticesduration <- c(0, lapply(index[-1], function(y){
        trackindicesvisits[y,1] - trackindicesvisits[y-1,2]
      }))

      # define an aggregation index
      iter <- 1
      aggregatedvisits <- rep(0, length(index))
      for(i in seq_along(intersticesduration)){
        if(intersticesduration[i] < tmaxinterstices/timeinterval){
          aggregatedvisits[i] <- iter
        }else{
          iter <- iter+1
          aggregatedvisits[i] <- iter
        }
      }

      trackindicesvisits1 <- cbind(trackindicesvisits[index,], aggregatedvisits)
      colnames(trackindicesvisits1) <- rep("", ncol(trackindicesvisits1))
      return(trackindicesvisits1)

    }

  }))

}

# function in order to classify visits as long-term visit (campsite) or short-term visit
classifyVisits <- function(trackindicesvisits, tmin, timeinterval){

  cbind(trackindicesvisits,
        ifelse(trackindicesvisits[,5] >= tmin/timeinterval, 1, 0))

}

# function in order to count the number of repeated long-term visits at each location
countAllReapeatedLongTermVisits <- function(trackindicesvisits, locations){

  cbind(trackindicesvisits, do.call(c, lapply(unique(locations), function(x){

    index <- which(locations == x)
    iter <- 1
    repeatedlongtermvisit <- rep(0, length(index))
    if(trackindicesvisits[index[1], 6] == 1){
      repeatedlongtermvisit[1] <- 1
    }
    for(i in seq_along(index)[-1]){

      if(trackindicesvisits[index[i], 6] == 1){
        if(trackindicesvisits[index[i], 4] != trackindicesvisits[index[i]-1, 4]){
          iter <- iter + 1
        }
        repeatedlongtermvisit[i] <- iter
      }

    }

    return(repeatedlongtermvisit)

  })))

}
