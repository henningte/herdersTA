#' @importFrom Rdpack reprompt
#' @import spacetime
#' @import lubridate
#' @import trajectories
#' @import rgdal
#' @import sp
#' @import rgeos
NULL

#' Aggregates a \code{\link[trajectories:Track-class]{Track}} object to Daily Resolution and to Locations.
#'
#' \code{aggregateDailyLocationsTrack} aggregates the data values
#' of a \code{\link[trajectories:Track-class]{Track}} object that has been
#' processed with \code{\link{locationsTrack}} to daily resolution.
#' Additionally, the funciton aggregates longitude, latitude and
#' altitude values to locations that were identified with
#' \code{\link{locationsTrack}} and determines the proportion of
#' gap values for each day with respect to the time frame defined
#' in \code{attributes(currenttrack)$night} that was set during
#' performing \code{\link{locationsTrack}}. The aggregation prioritises
#' campsites
#' over short-term visits. This means that if during one day, there are
#' values for a campsite and a short-term visit, values will be
#' aggregated only for the campsite. If for a day there are vales for
#' more than one campsite, the function considerd the campsite with most
#' data values.
#'
#' The function does not work with \code{\link{locationsTrack}} objects
#' with a temporal resolution greater than sub-daily resolution.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object.
#' @param crs A character string describing a projection and datum
#' in the \code{PROJ.4} format (see \code{\link[rgdal]{projInfo}}).
#' @return An object of class \code{\link[trajectories:Track-class]{Track}} that represents the
#' input \code{\link[trajectories:Track-class]{Track}} object (\code{currenttrack}) that is aggregated
#' to a daily resolution and for which longitude, latitude and altitude data represent
#' centroids for the location that has the main data values for that day that were assigned
#' to a location.
#'
#' @seealso \code{\link{reorganizeTrack}}.
#' @examples #
#' @export
aggregateDailyLocationsTrack <- function(currenttrack, crs){

  # check if currenttrack is of class Track
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack must be of class Track\n")
  }

  # check if currenttrack has an attribute night
  if(!any(names(attributes(currenttrack)) == "night")){
    stop("currenttrack must have an attribute 'night' as returned if processed with locationsTrack\n")
  }

  # define a logical vector identical to night
  night <- attributes(currenttrack)$night

  # transform currenttrack to a SPDF
  trsSP <- TrackToSpatialPointsDataFrame(currenttrack = currenttrack, toproject = TRUE, crs = crs)

  # compute centroid longitude, latitude and altitude values for each location
  locationscentroidpositions <- as.data.frame(do.call(rbind, lapply(unique(trsSP@data$location), function(x){

    # index for data values corresponding to the current location
    indexlocation <- which(trsSP$location[night] == x)

    # median of the longitude, latitude, altitude
    c(x, apply(matrix(trsSP@coords[indexlocation,], ncol = 2), 2, median), median(na.omit(as.numeric(trsSP$HEIGHT[indexlocation]))))
  })), stringsAsFactors = FALSE)
  colnames(locationscentroidpositions) <- c("location", "longitude", "latitude", "altitude")

  # extract the days from trsSP
  days <- strftime(trsSP$time[night], "%Y-%m-%d")

  # aggregate the value on a daily basis
  aggregatedcurrenttracksdata <- do.call(rbind, lapply(unique(days), function(x){

    x <- seq_len(nrow(trsSP@data[night,]))[days == x]

    # define an index for nogap values
    indexnogaps <- which(!as.logical(trsSP@data$gap[night][x]))

    # extract if there was an arrival or departure (with respect to a campsite)
    arrival <- any(ifelse(na.omit(trsSP@data$campsite[night][x]) == 1 & na.omit(trsSP@data$arrived[night][x]) == TRUE, TRUE, FALSE))
    departure <- any(ifelse(na.omit(trsSP@data$campsite[night][x]) == 1 & na.omit(trsSP@data$left[night][x]) == TRUE, TRUE, FALSE))

    # determine the proportion of nogaps
    nogapsproportion <- length(indexnogaps)/length(x)

    # determine if campsites are present
    campsitespresent <- ifelse(any(na.omit(trsSP@data$campsite[night][x]) == 1), TRUE, FALSE)

    # define an index of values to consider (if campsites are present or not)
    if(campsitespresent == TRUE){
      indexcampsitevalues <- which(trsSP@data$campsite[night][x] == 1)
    }else{
      indexcampsitevalues <- seq_len(length(trsSP@data$campsite[night][x]))
    }

    # determine the location with the most number of values
    valuesperlocation <- sapply(unique(trsSP@data$location[night][x][indexcampsitevalues]), function(y){
      length(which(trsSP@data$location[night][x][indexcampsitevalues] == y))
    })
    if(any(names(valuesperlocation) != 0)){
      valuesperlocation <- valuesperlocation[names(valuesperlocation) != 0]
    }
    targetlocation <- names(valuesperlocation)[which.max(valuesperlocation)]

    # extract the number of visit
    numberofvisit <- trsSP@data$visitscampsite[night][x][trsSP@data$location[night][x] == targetlocation][1]

    # correct all values if there is no campsite
    if(campsitespresent == FALSE){
      targetlocation <- "0"
      arrival <- FALSE
      departure <- FALSE
      numberofvisit <- NA
      nogapsproportion <- 0
    }

    # extract the centroid position values
    c(days[x[1]], targetlocation, as.numeric(locationscentroidpositions[locationscentroidpositions$location == targetlocation,-1]), arrival, departure, numberofvisit, nogapsproportion)

  }))
  aggregatedcurrenttracksdata <- as.data.frame(aggregatedcurrenttracksdata, stringsAsFactors = FALSE)
  colnames(aggregatedcurrenttracksdata) <- c("day", "location", "longitude", "latitude", "altitude", "arrived", "left", "visitscampsite", "nogapsproportion")
  aggregatedcurrenttracksdata[,-c(1, 6, 7)] <- apply(aggregatedcurrenttracksdata[,-c(1, 6, 7)], 2, as.numeric)

  # redefine the lon/lat data of gaps to the respective previous location
  gapsblocks <- identifyBlocksVariable(currenttrack = aggregatedcurrenttracksdata, variable = "location", value = 0)

  # if there are gaps
  if(!is.null(gapsblocks)){

    # remove the first block if present (there is no previous location)
    if(gapsblocks[1,1] == 1){

      # insert the coordinates of the next campsite (to avoid confusion when summarising)
      aggregatedcurrenttracksdata$longitude[gapsblocks[1,1]:gapsblocks[1,2]] <- rep(aggregatedcurrenttracksdata$longitude[gapsblocks[1,2]+1], length = gapsblocks[1,1]:gapsblocks[1,2])
      aggregatedcurrenttracksdata$latitude[gapsblocks[1,1]:gapsblocks[1,2]] <- rep(aggregatedcurrenttracksdata$latitude[gapsblocks[1,2]+1], length = gapsblocks[1,1]:gapsblocks[1,2])

      # remove the first block from gapsblocks
      gapsblocks <- gapsblocks[-1,]
    }

    # for remaining blocks, insert the lon/lat data of the previous location (that relates always to a campsite visit)
    for(i in seq_len(nrow(gapsblocks))){
      aggregatedcurrenttracksdata$longitude[gapsblocks[i,1]:gapsblocks[i,2]] <- rep(aggregatedcurrenttracksdata$longitude[gapsblocks[i,1]-1], length = gapsblocks[i,1]:gapsblocks[i,2])
      aggregatedcurrenttracksdata$latitude[gapsblocks[i,1]:gapsblocks[i,2]] <- rep(aggregatedcurrenttracksdata$latitude[gapsblocks[i,1]-1], length = gapsblocks[i,1]:gapsblocks[i,2])
    }

  }

  # define the time vector of the new Track object
  newtime <- as.POSIXct(aggregatedcurrenttracksdata$day, format = "%Y-%m-%d")

  # create a new Track object
  Track(track = STIDF(
    sp = SpatialPoints(coords =
                         data.frame(lon = aggregatedcurrenttracksdata$longitude,
                                    lat = aggregatedcurrenttracksdata$latitude),
                       proj4string = CRS(proj4string(currenttrack@sp))),
    time = newtime,
    endTime = newtime,
    data = aggregatedcurrenttracksdata)
        )

}
