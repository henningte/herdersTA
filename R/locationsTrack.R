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
#' (campsites) or short-term visits. During the identification of
#' clusters, data values within a user defined daily time interval
#' are considered. The remaining values are set to the next
#' location identified for the adjacent time intervals. This
#' procedure can be used in order to identify campsites by
#' assuming that a household has a campsite where it stayed
#' over night. If values outside the defined time interval
#' have a too far distance, they are classified as short-term
#' visit.
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
#' @param night An integer vector with two elements:
#' \enumerate{
#'   \item The first element specifies the start hour of the night, e.g. \code{0}
#'   for 0 o'clock.
#'   \item The first element specifies the start hour of the night, e.g. \code{4}
#'   for 4 o'clock.
#' }
#' @param crs A character string describing a projection and datum
#' in the \code{PROJ.4} format (see \code{\link[rgdal]{projInfo}}).
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
                           night = c(16, 20),
                           crs = "+proj=utm +zone=46 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                           summary = TRUE){

  # convert track to SpatialPointsDataFrame and transform to UTM
  trsSP <- TrackToSpatialPointsDataFrame(currenttrack, crs, toproject = TRUE)

  # append the information on whether a data value was ecorded during night or day
  currenttrack <- classifyNightTrack(currenttrack, night)
  attributes(trsSP)$night <- attributes(currenttrack)$night

  # cluster the data points
  trsSP$location <- extractClustersBuffer(trsSP, radius)

  # return NULL if a data set contains only one location (no Track object can be constructed from one point)
  if(length(unique(trsSP$location)) == 1){
    warning("Only one location identified!")
    return(NULL)
  }

  # redefine indices of locations (according to arrival time)
  trsSP@data <- redefineIndices(df = trsSP@data, indices = "location", time = "time", notchange = 0)

  # extract the data value indices of visits at each location
  trackindicesvisits <- do.call(rbind, lapply(unique(trsSP$location), function(x){

    # get the block start and end indices for the current location
    blockindices <- identifyBlocksVariable(currenttrack = trsSP, variable = "location", value = x)

    row.names(blockindices) <- rep(x, nrow(blockindices))
    return(cbind(x, blockindices))

  }))

  # aggregate values at the same locations to blocks if interstices between them are < tmaxinterstices
  trackindicesvisits <- aggregateRepeatedVisits(trackindicesvisits, locations = trackindicesvisits[,1], intersticesduration, tmaxinterstices, timeinterval)

  # get an index of aggregated blocks
  indexaggregatedvisits <- paste0(trackindicesvisits[,1], "_", trackindicesvisits[,4])

  # compute the duration of the aggregated visits
  blockduration <- tapply(seq_len(nrow(trackindicesvisits)), indexaggregatedvisits, function(x){
    block <- as.vector(trackindicesvisits[x,c(2,3)])
    (block[length(block)] - block[1])
  })
  trackindicesvisits <- cbind(trackindicesvisits, duration = do.call("c", lapply(unique(indexaggregatedvisits), function(x) rep(blockduration[names(blockduration) == x], length(which(indexaggregatedvisits == x))))))

  # classify visits as long-term visit (campsite) or short-term visit
  trackindicesvisits <- classifyVisits(trackindicesvisits, tmin, timeinterval)

  # count the number of repeated long-term visits at each location
  trackindicesvisits <- countAllReapeatedLongTermVisits(trackindicesvisits, locations = trackindicesvisits[,1])
  # does not work if there is a repeated visit at the same location, but the previous visit was not a campsite 20181203

  # order indexaggregatedvisits
  indexaggregatedvisits <- indexaggregatedvisits[order(trackindicesvisits[,2])]

  # order the entries of trackindicesvisits
  trackindicesvisits <- trackindicesvisits[order(trackindicesvisits[,2]),]
  trackindicesvisits <- cbind(trackindicesvisits, indexaggregatedvisits)

  # identify adjacent visits of locations
  nextvisits <- do.call(rbind, lapply(unique(indexaggregatedvisits[trackindicesvisits[,1] != 0]), function(y){

    x <- seq_len(nrow(trackindicesvisits))[trackindicesvisits[,1] != 0][indexaggregatedvisits[trackindicesvisits[,1] != 0] == y]

    # define the row index range in trackindicesvisits
    blockindicesrange <- range(x)

    # search for the next adjacent block that is not a gap (i.e. location != 0)
    # search the next previous block
    isgap <- TRUE
    iter <- 1
    if(blockindicesrange[1] == 1){
      # if the block is the first value in the track, there is no previous block
      previousblock <- NA
    }else{
      while(isgap){
        if(trackindicesvisits[blockindicesrange[1]-iter, 1] != 0){
          # if the value iter before is not a gap, set previousblock to the data value index in currenttrack that is the last data value of this previous block
          previousblock <- trackindicesvisits[blockindicesrange[1]-iter, 3]
          isgap <- FALSE
        }else{
          if(blockindicesrange[1]-iter == 1){
            # if the next previous value is the first value, there is no previous block
            previousblock <- NA
            isgap <- FALSE
          }
          iter <- iter + 1
        }
      }
    }

    # search the next following block
    isgap <- TRUE
    iter <- 1
    if(blockindicesrange[2] == nrow(trackindicesvisits)){
      # if the block is the last value in the track, there is no next foloowing block
      nextblock <- NA
    }else{
      while(isgap){
        if(trackindicesvisits[blockindicesrange[2]+iter, 1] != 0){
          # if the value iter before is not a gap, set nextblock to the data value index in currenttrack that is the first data value of this next block
          nextblock <- trackindicesvisits[blockindicesrange[2]+iter, 2]
          isgap <- FALSE
        }else{
          if(blockindicesrange[2]+iter == nrow(trackindicesvisits)){
            # if the next following value is the last value, there is no next following block
            nextblock <- NA
            isgap <- FALSE
          }
          iter <- iter + 1
        }
      }
    }

    data.frame(indexaggregatedvisits = y, blockstart = trackindicesvisits[blockindicesrange[1], 3], blockend = trackindicesvisits[blockindicesrange[2], 2], previousblock = previousblock, nextblock = nextblock)

  }))
  nextvisits[,-1] <- apply(nextvisits[,-1], 2, function(x) as.numeric(as.character(x)))

  # check if time interval between two visits at different locations is < tmaxintersites (i.e. it is assumed that it can be determined of a household arrived or left a location)
  nextvisits$arrivaldet <- ifelse((nextvisits$blockstart - nextvisits$previousblock) * timeinterval < tmaxinterstices, TRUE, FALSE)
  nextvisits$departuredet <- ifelse((nextvisits$nextblock - nextvisits$blockend) * timeinterval < tmaxinterstices, TRUE, FALSE)

  # detect if the number of repeated visits should be set to NA because it is considered unreliable
  nextvisitlocation <- sapply(strsplit(as.character(nextvisits[,1]), split = "_"), function(x) x[1])
  nextvisitvisit <- sapply(strsplit(as.character(nextvisits[,1]), split = "_"), function(x) x[2])
  nextvisits$repeatedvisitsreliable <- c(TRUE, sapply(seq_len(nrow(nextvisits))[-1], function(x){
    if(nextvisitlocation[x] == nextvisitlocation[x-1]){
      FALSE
    }else{
      TRUE
    }
  }))

  # add all values to the input track object
  tracklocationsvisits <- do.call(rbind, lapply(seq_len(nrow(trackindicesvisits)), function(x){

    do.call(rbind, lapply(trackindicesvisits[x, 2]:trackindicesvisits[x, 3], function(y){
      trackindicesvisits[x, c(1, 6, 7, 8)]
    }))

  }))
  currenttrack$location <- tracklocationsvisits[,1]
  currenttrack$campsite <- tracklocationsvisits[,2]
  currenttrack$visitscampsite <- tracklocationsvisits[,3]
  currenttrack$visitscampsite[currenttrack$location %in% nextvisitlocation[nextvisits$repeatedvisitsreliable == FALSE] & currenttrack$visitscampsite %in% nextvisitvisit[nextvisits$repeatedvisitsreliable == FALSE]] <- NA
  currenttrack$indexaggregatedvisits <- tracklocationsvisits[,4]

  # add information to currenttrack on departures and arrivals
  currenttrack$arrived <- rep(FALSE, nrow(currenttrack@data))
  currenttrack$arrived[nextvisits$blockstart[nextvisits$arrivaldet == TRUE]] <- TRUE
  currenttrack$left <- rep(FALSE, nrow(currenttrack@data))
  currenttrack$left[nextvisits$blockend[nextvisits$departuredet == TRUE]] <- TRUE

  # set the number of campsite visits to NA for short-term visits
  currenttrack$visitscampsite[currenttrack$campsite == 0] <- NA

  # set the values for location, campsite, arrived, left and visitscampsite to NA for gaps
  currenttrack@data[which(currenttrack$location == 0),(ncol(currenttrack@data)-4):ncol(currenttrack@data)] <- NA

  # summarise the values for each visit and location
  if(summary == TRUE){

    # define an index for each location and campsite
    # indexvisitlocation <- paste0(currenttrack$location, "_", currenttrack$visitscampsite)
    indexvisitlocation <- currenttrack@data$indexaggregatedvisits
    indexvisitlocation[is.na(indexvisitlocation)] <- "0_NA"

    # compute mean coordinates for each visit
    coordinatesvisitlocation <- SpatialPoints(t(sapply(unique(indexvisitlocation), function(x) c(mean(trsSP@coords[indexvisitlocation == x,1]), mean(trsSP@coords[indexvisitlocation == x,2])))), proj4string = trsSP@proj4string)
    coordinatesvisitlocation <- spTransform(coordinatesvisitlocation, currenttrack@sp@proj4string)@coords

    summaryvisitslocation <- data.frame(
      location = sapply(unique(indexvisitlocation), function(x) currenttrack$location[indexvisitlocation == x][1]),
      lon = coordinatesvisitlocation[,1],
      lat = coordinatesvisitlocation[,2],
      alt = sapply(unique(indexvisitlocation), function(x) median(as.numeric(currenttrack$HEIGHT[indexvisitlocation == x]))),
      campsite = sapply(unique(indexvisitlocation), function(x) currenttrack$campsite[indexvisitlocation == x][1]),
      visitscampsite = sapply(unique(indexvisitlocation), function(x) currenttrack$visitscampsite[indexvisitlocation == x][1]),
      arrivaltime = as.POSIXct(sapply(unique(indexvisitlocation), function(x) currenttrack$time[indexvisitlocation == x][1]), origin = "1970-01-01 00:00:00"),
      departuretime = as.POSIXct(sapply(unique(indexvisitlocation), function(x) currenttrack$time[indexvisitlocation == x][length(which(indexvisitlocation == x))]), origin = "1970-01-01 00:00:00"),
      residencetime = sapply(unique(indexvisitlocation), function(x) difftime(currenttrack$time[indexvisitlocation == x][length(which(indexvisitlocation == x))], currenttrack$time[indexvisitlocation == x][1], units = "sec")),
      speed = sapply(unique(indexvisitlocation), function(x) mean(as.numeric(currenttrack$SPEED[indexvisitlocation == x])))
    )

    # summaryvisitslocation[-is.na(summaryvisitslocation$location),]
    summaryvisitslocation[summaryvisitslocation$location != 0,]

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
aggregateRepeatedVisits <- function(trackindicesvisits, locations, intersticesduration, tmaxinterstices, timeinterval){

  do.call(rbind, lapply(unique(locations), function(x){

    # get entries of trackindicesvisits of location x
    index <- which(locations == x)

    if(length(index) == 1){
      matrix(c(trackindicesvisits[index,], 1), nrow = 1, dimnames = list(x, NULL))
    }else{

      # get the duration of interstices (add a 0 for the first visit)
      intersticesduration <- c(0, lapply(index[-1], function(y){
        trackindicesvisits[y,2] - trackindicesvisits[y-1,3]
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

# function in order to define indices representing the aggregation of visits for campsites
aggregateRepeatedCampsiteVisits <- function(trackindicesvisits){

  trackindicesvisits$newaggregation <- rep(0, nrow(trackindicesvisits))
  for(i in unique(trackindicesvisits[,8])[-1]){

    index <- trackindicesvisits[,8] == i

    cond <- which(trackindicesvisits[,6] == 1 & index)

    if(length(cond) > 0){

      listaggregations <- tapply(cond, trackindicesvisits[cond,4], function(x) x)

      for(j in seq_along(listaggregations)){
        trackindicesvisits$newaggregation[listaggregations[[j]][1]:listaggregations[[j]][length(listaggregations[[j]])]] <- paste0(i, "_", j)
      }

    }

  }

  # return trackindicesvisits
  return(trackindicesvisits)

}

# function in order to identify arrivals at/departures from a campsite
movedTrack <- function(currenttrack){

  # identify arrivals
  arrival <- c(0, sapply(seq_len(nrow(currenttrack@data))[-1], function(x){
    if(currenttrack@data$aggregationcampsite[x] != currenttrack@data$aggregationcampsite[x-1] && !is.na(currenttrack@data$aggregationcampsite[x]) && !is.na(currenttrack@data$aggregationcampsite[x-1])){
      TRUE
    }else{
      FALSE
    }
  }))

  # identify departures
  departure <- rep(FALSE, length(arrival))
  departure[which(arrival)-1] <- TRUE

  # add arrival and departure to currenttrack
  currenttrack$arrival <- arrival
  currenttrack$departure <- departure

  # remove currenttrack$aggregationcampsite
  currenttrack$aggregationcampsite <- NULL

  # return currenttrack
  return(currenttrack)

}
