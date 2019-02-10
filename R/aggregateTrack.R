#' @importFrom Rdpack reprompt
#' @importFrom plyr join
#' @importFrom data.table rbindlist
#' @importFrom tidyr fill
#' @importFrom sp coordinates SpatialPoints
#' @importFrom spacetime STIDF
#' @importFrom trajectories Track
NULL

#' Aggregates Tracks
#'
#' \code{aggregate.trackvisits} temporally aggregates objects of class
#' \code{\link[trajectories:Track-class]{Track}}. Aggregation means that values
#' of a \code{\link[trajectories:Track-class]{Track}} object are aggregated
#' temporally, e.g. from hourly to daily values. The function assumes that
#' certain variables are present for the \code{\link[trajectories:Track-class]{Track}}
#' object, specifically \code{location}, \code{group}, \code{altitude}, \code{gap},
#' \code{campsite}, \code{norepeatedvisits}, \code{start} and \code{end} as
#' defined in \code{\link{locationsTracks}}.
#'
#' @param x An object of class \code{\link[trajectories:Track-class]{Track}} that
#' has been processed with \code{\link{reorganizeTracks}}, \code{\link{fillGapTracksNight}}
#' and \code{\link{locationsTrack}}, so that it contains the variables \code{location},
#' \code{group}, \code{altitude}, \code{gap}, \code{campsite}, \code{norepeatedvisits},
#' \code{start} and \code{end}. All other variables will be ignored.
#' @param by A vector with grouping values for each data value in \code{x}
#' corresponding to the time. This can be for example the day of each given
#' \code{POSIXct} value in order to aggregate to daily values.
#' @return The temporally aggregated \code{x}.
#'
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{redefineIndices}},
#' \code{\link{fillGapTrack}}, \code{\link{fillGapTracks}},
#' \code{\link{extractClustersBuffer}}.
#' @examples #
#' @export
aggregateTrack <- function(x, by = list(strftime(x@time, format = "%Y-%m-%d"))){

  # checks
  if(!(inherits(x, "Track"))){
    stop("x must be of class Track\n")
  }

  #### aggregation of existing variables ####

  # index of non-gap groups
  nongapgroupsindex <- which(x$location != 0)

  # collect any group (visit) related data
  groupdata <- data.table::rbindlist(tapply(seq_len(nrow(x@data))[nongapgroupsindex], x$group[nongapgroupsindex], function(y){

    data.frame(group = x$group[y[1]], location = x$location[y[1]], campsite = x$campsite[y[1]], norepeatedcampsitevisits = x$norepeatedcampsitevisits[y[1]], longitude = sp::coordinates(x@sp)[y[1],1], latitude = sp::coordinates(x@sp)[y[1],2], altitude = x$altitude[y[1],])

  }, simplify = FALSE))

  # convert by to POSIXct
  by[[1]] <- as.POSIXct(by[[1]], tz = tzone(x@time))

  # compute aggregated time values
  agg <- data.frame(time = unique(by[[1]]))

  # aggregate group
  agg$group <- as.numeric(tapply(x$group, by[[1]], function(y){

    if(any(y != 0)){

      # table
      grouptable <- table(y[y != 0])

      # get the group with the most values
      names(grouptable[which.max(grouptable)])

    }else{
      0
    }

  }))

  # aggregate other variables
  agg <- plyr::join(x = agg, y = groupdata, by = "group", type = "left")

  # fill NA locations with 0 (gaps)
  agg$location[is.na(agg$location)] <- 0

  # aggregate start and end
  agg$start <- !duplicated(x = agg$group, fromLast = FALSE) & agg$group != 0
  agg$end <- !duplicated(x = agg$group, fromLast = TRUE) & agg$group != 0

  # aggregate gaps
  agg$gap <- tapply(seq_len(nrow(x@data)), by[[1]], function(y){

    if(any(!x$gap[y])){
      FALSE # no gap
    }else{
      TRUE
    }

  })

  # aggregate filled
  agg$filled <- tapply(seq_len(nrow(x@data)), by[[1]], function(y){

    if(any(!x$gap[y] & !x$filled[y])){
      FALSE # at least one not filled non-gap
    }else{
      if(any(x$filled[y])){
        TRUE # at least one filled non-gap
      }else{
        FALSE # only non-filled gaps.
      }
    }

  })

  # set agg$campsite for gaps to FALSE
  agg$campsite[agg$gap] <- FALSE

  #### adjusting the location position for gaps ####

  # extract the coordinates
  aggcoords <- agg[,c("longitude", "latitude", "altitude")]

  # fill gaps with dummy values (first from next visit for the previous gap and than from the previous visit for the next gab for the ending gap)
  aggcoords <- tidyr::fill(aggcoords, seq_len(ncol(aggcoords)), .direction = "up")
  aggcoords <- tidyr::fill(aggcoords, seq_len(ncol(aggcoords)), .direction = "down")

  # insert these values in agg
  agg$longitude <- aggcoords$longitude
  agg$latitude <- aggcoords$latitude
  agg$altitude <- aggcoords$altitude

  # construct a SpatialPoints object
  aggcoords <- sp::SpatialPoints(coords = aggcoords[,1:2], proj4string = CRS(proj4string(x)))

  #### computation of new variables ####

  # gapdurationnextcampsite_wos
  agg$gapdurationnextcampsite_wos <- gapdurationnextcampsite_wos(agg)

  # gapdurationnextcampsite_wis
  agg$gapdurationnextcampsite_wis <- gapdurationnextcampsite_wis(agg)

  # distancenextvisit_wis
  agg$distancenextvisit_wis <- distancenextvisit_wis(agg = agg, aggcoords = aggcoords)

  # distancenextvisit_wos
  agg$distancenextvisit_wos <- distancenextvisit_wos(agg = agg, aggcoords = aggcoords)

  # distancenextvisit_wis_campsite
  agg$distancenextvisit_wis_campsite <- distancenextvisit_wis_campsite(agg = agg)

  # altitudinaldistancenextvisit_wis
  agg$altitudinaldistancenextvisit_wis <- altitudinaldistancenextvisit_wis(agg = agg, aggcoords = aggcoords)

  # altitudinaldistancenextvisit_wos
  agg$altitudinaldistancenextvisit_wos <- altitudinaldistancenextvisit_wos(agg = agg, aggcoords = aggcoords)

  # altitudinaldistancenextvisit_wis_campsite
  agg$altitudinaldistancenextvisit_wis_campsite <- altitudinaldistancenextvisit_wis_campsite(agg = agg)

  #### creation of the Track object ####

  # create the aggregated Track object
  trajectories::Track(spacetime::STIDF(sp = sp::SpatialPoints(coords = aggcoords, proj4string = CRS(proj4string(x))), time = agg$time, data = agg[,!(colnames(agg) %in% c("time", "longitude", "latitude"))], endTime = agg$time))

}

#' Computes Gap Durations Ignoring Short-Term Visits
#'
#' \code{gapdurationnextcampsite_wos} computes for a \code{data.frame} as created
#' internally in \code{\link{aggregateTrack}} the duration of gaps as number of
#' gap data values between campsite visits, whereby short-term visits are counted as
#' gaps.
#'
#' @param agg A \code{data.frame} object as created internally in \code{\link{aggregateTrack}}.
#' @return A numeric vector containing the duration of gaps between campsites
#' as number of gap data values between campsite visits, whereby short-term
#' visits are counted as gaps. Values for a following
#' move are always assigned to the last value of a
#' campsite visit in \code{agg} as indicated by \code{agg$end & agg$campsite}.
#'
#' @seealso \code{\link{aggregateTrack}}
#' @examples #
#' @keywords internal
#' @export
gapdurationnextcampsite_wos <- function(agg){

  # extract all indices of campsite ends
  campsiteends <- which(agg$end)

  # create a dummy vector with duration values of the next non-campsite value blocks
  noncampsitesduration <- rep(0, nrow(agg))

  # extract all non-campsite value blocks
  noncampsites <- identifyBlocksVariable(currenttrack = agg, variable = "campsite", value = FALSE)

  # discard the leading non-campsite value block (value cannot be assigned)
  noncampsites <- noncampsites[noncampsites$start != 1,]

  # discard all blocks with no previous campsite (value cannot be assigned)
  indexnopreviouscampsite <- which(sapply(noncampsites$start, function(x) any(campsiteends < x)) > 0)
  if(length(indexnopreviouscampsite) > 0){
    noncampsites <- noncampsites[sapply(noncampsites$start, function(x) any(campsiteends < x)),]
  }

  if(nrow(noncampsites) > 0){

    # get for each gap the nearest end of a campsite
    nearestvaluebeforegap <- sapply(noncampsites$start, function(x){

      # get all campsiteends < x
      campsiteends_before <- campsiteends[campsiteends < x]

      # get the nearest index
      campsiteends_before[which.min(abs(campsiteends_before - x))]

    })

    # get the length of non-campsite value blocks (duration in days)
    noncampsites$duration <- apply(noncampsites, 1, function(x) length(x[1]:x[2]))

    # insert values for the departure date prior the non-campsite block
    noncampsitesduration[nearestvaluebeforegap] <- noncampsites$duration

  }

  # return noncampsitesduration
  return(noncampsitesduration)

}

#' Computes Gap Durations Not Ignoring Short-Term Visits
#'
#' \code{gapdurationnextcampsite_wis} computes for a \code{data.frame} as created
#' internally in \code{\link{aggregateTrack}} the duration of gaps as number of
#' gap data values between visits, whereby short-term visits are not counted as
#' gaps and are considered as visits.
#'
#' @param agg A \code{data.frame} object as created internally in \code{\link{aggregateTrack}}.
#' @return A numeric vector containing the duration of gaps between visits
#' as number of gap data values between visits, whereby short-term visits are not counted as
#' gaps and are considered as visits. Values for a following
#' move are always assigned to the last value of a
#' visit in \code{agg} as indicated by \code{agg$end}.
#'
#' @seealso \code{\link{aggregateTrack}}
#' @examples #
#' @keywords internal
#' @export
gapdurationnextcampsite_wis <- function(agg){

  # extract all indices of campsite ends
  campsiteends <- which(agg$end)

  # create a dummy vector with duration values of the next gap value blocks
  gapsduration <- rep(0, nrow(agg))

  # extract all gap value blocks
  gaps <- identifyBlocksVariable(currenttrack = agg, variable = "gap", value = TRUE)

  # discard the leading gap value block (value cannot be assigned)
  gaps <- gaps[gaps$start != 1,]

  # discard all blocks with no previous campsite (value cannot be assigned)
  indexnopreviouscampsite <- which(sapply(gaps$start, function(x) any(campsiteends < x)) > 0)
  if(length(indexnopreviouscampsite) > 0){
    gaps <- gaps[sapply(gaps$start, function(x) any(campsiteends < x)),]
  }

  if(nrow(gaps) > 0){

    # get for each gap the nearest end of a campsite
    nearestvaluebeforegap <- sapply(gaps$start, function(x){

      # get all campsiteends < x
      campsiteends_before <- campsiteends[campsiteends < x]

      # get the nearest index
      campsiteends_before[which.min(abs(campsiteends_before - x))]

    })

    # get the length of gap value blocks (duration in days)
    gaps$duration <- apply(gaps, 1, function(x) length(x[1]:x[2]))

    # insert values for the departure date prior the gap block
    gapsduration[nearestvaluebeforegap] <- gaps$duration

  }

  # return gapsduration
  return(gapsduration)

}


#' Computes Move Distances Not Ignoring Short-Term Visits
#'
#' \code{distancenextvisit_wis} computes for a \code{data.frame} as created
#' internally in \code{\link{aggregateTrack}} the linear distance between locations
#' of adjacent visits, whereby short-term visits are not counted as gaps and
#' are considered as visits.
#'
#' @param agg A \code{data.frame} object as created internally in \code{\link{aggregateTrack}}.
#' @param aggcoords A \code{\link[sp]{SpatialPoints}} object with a value for each value
#' in \code{agg}.
#' @return A numeric vector containing the linear distance between locations of adjacent visits,
#' whereby short-term visits are not counted as gaps and are considered as visits. Values for a following
#' move
#' are always assigned to the last value of a visit in \code{agg} as indicated by \code{agg$end}.
#'
#' @seealso \code{\link{aggregateTrack}}
#' @examples #
#' @keywords internal
#' @export
distancenextvisit_wis <- function(agg, aggcoords){

  # create a dummy vector with distances between adjacent locations
  gapsdistance <- rep(0, nrow(agg))

  # extract all location blocks
  locationblocks <- data.table::rleidv(agg, "location")

  # identify start and end values
  locationblocks <- data.table::rbindlist(tapply(seq_len(nrow(agg)), locationblocks, function(x){
    data.frame(start = x[1], end = x[length(x)])
  }))

  # discard the leading block (value cannot be assigned)
  locationblocks <- locationblocks[locationblocks$start != 1,]

  if(nrow(locationblocks) > 0){

    # extract for each gap the coordinates of the first point prior the gap and the first point of the gap
    points1 <- aggcoords[locationblocks$start-1,]
    points2 <- aggcoords[locationblocks$start,]

    # get the distance between points1 and points2
    locationblocks$distance <- sp::spDists(x = points1, y = points2, longlat = FALSE, diagonal = TRUE)

    # insert values for the departure date prior the gap block
    gapsdistance[locationblocks$start - 1] <- locationblocks$distance

  }

  # return gapsdistance
  return(gapsdistance)

}

#' Computes Move Distances Not Ignoring Short-Term Visits
#'
#' \code{distancenextvisit_wos} computes for a \code{data.frame} as created
#' internally in \code{\link{aggregateTrack}} the linear distance between locations
#' of adjacent campsites, whereby short-term visits are counted as gaps.
#'
#' @param agg A \code{data.frame} object as created internally in \code{\link{aggregateTrack}}.
#' @param aggcoords A \code{\link[sp]{SpatialPoints}} object with a value for each value
#' in \code{agg}.
#' @return A numeric vector containing the linear distance between locations of adjacent campsites,
#' whereby short-term visits are counted as gaps. Values for a following
#' move
#' are always assigned to the last value of a visit in \code{agg} as indicated by \code{agg$end}.
#'
#' @seealso \code{\link{aggregateTrack}}
#' @examples #
#' @keywords internal
#' @export
distancenextvisit_wos <- function(agg, aggcoords){

  # extract all indices of campsite ends
  campsiteends <- which(agg$end & agg$campsite)

  # create a dummy vector with distances between adjacent locations
  gapsdistance <- rep(0, nrow(agg))

  # index for campsite visits
  campsiteindex <- which(agg$campsite)

  # extract all location blocks
  locationblocks <- data.table::rleidv(agg[campsiteindex,], "location")

  # identify start and end values
  locationblocks <- data.table::rbindlist(tapply(seq_len(nrow(agg))[campsiteindex], locationblocks, function(x){
    data.frame(start = x[1], end = x[length(x)])
  }))

  # discard the leading block (value cannot be assigned)
  locationblocks <- locationblocks[locationblocks$start != 1,]

  # discard all blocks with no previous campsite (value cannot be assigned)
  indexnopreviouscampsite <- which(sapply(locationblocks$start, function(x) any(campsiteends < x)) > 0)
  if(length(indexnopreviouscampsite) > 0){
    locationblocks <- locationblocks[sapply(locationblocks$start, function(x) any(campsiteends < x)),]
  }

  if(nrow(locationblocks) > 1){

    # get for each location block the nearest end of a campsite
    nearestvaluebeforegap <- sapply(locationblocks$start, function(x){

      # get all campsiteends < x
      campsiteends_before <- campsiteends[campsiteends < x]

      # get the nearest index
      campsiteends_before[which.min(abs(campsiteends_before - x))]

    })

    # extract for each gap the coordinates of the first point prior the gap and the first point of the gap
    points1 <- aggcoords[locationblocks$start-1,]
    points2 <- aggcoords[locationblocks$start,]

    # get the distance between points1 and points2
    locationblocks$distance <- sp::spDists(x = points1, y = points2, longlat = FALSE, diagonal = TRUE)

    # insert values for the departure date prior the gap block
    gapsdistance[nearestvaluebeforegap] <- locationblocks$distance

  }

  # return gapsdistance
  return(gapsdistance)

}

#' Computes Move Distances Not Ignoring Short-Term Visits
#'
#' \code{distancenextvisit_wis_campsite} computes for a \code{data.frame} as created
#' internally in \code{\link{aggregateTrack}} the linear distance between locations
#' of adjacent campsites along locations of short-term visits (if present). This is done
#' by summing all distances covered between adjacent campsites as computed with
#' \code{\link{distancenextvisit_wis}}.
#'
#' @param agg A \code{data.frame} object as created internally in \code{\link{aggregateTrack}}.
#' @return A numeric vector containing the linear distance between locations
#' of adjacent campsites along locations of short-term visits (if present). Values for a following
#' move
#' are always assigned to the last value of a campsite in \code{agg} as indicated by \code{agg$end}.
#'
#' @seealso \code{\link{aggregateTrack}}
#' @examples #
#' @keywords internal
#' @export
distancenextvisit_wis_campsite <- function(agg){

  # create a dummy vector with distances between adjacent locations
  gapsdistance <- rep(0, nrow(agg))

  # index for campsite visits
  campsiteindex <- which(agg$campsite)

  # extract all location blocks
  locationblocks <- data.table::rleidv(agg[campsiteindex,], "location")

  # identify start and end values
  locationblocks <- data.table::rbindlist(tapply(seq_len(nrow(agg))[campsiteindex], locationblocks, function(x){
    data.frame(start = x[1], end = x[length(x)])
  }))

  # discard the leading block (value cannot be assigned)
  locationblocks <- locationblocks[locationblocks$start != 1,]

  # change the indices so that they display interstices between campsites
  locationblocks <- data.frame(start = locationblocks$end[-nrow(locationblocks)], end = locationblocks$start[-1])

  if(nrow(locationblocks) > 0){

    # get the distance between campsites along short-term visit locations
    locationblocks$distance <- apply(locationblocks, 1, function(x) sum(agg$distancenextvisit_wis[x[1]:x[2]]))

    # insert values for the departure date prior the non-campsite block
    gapsdistance[locationblocks$start] <- locationblocks$distance

  }

  # return gapsdistance
  return(gapsdistance)

}


#' Computes Altitudinal Move Distances Not Ignoring Short-Term Visits
#'
#' \code{altitudinaldistancenextvisit_wis} computes for a \code{data.frame} as created
#' internally in \code{\link{aggregateTrack}} the linear altitudinal distance between locations
#' of adjacent visits, whereby short-term visits are not counted as gaps and
#' are considered as visits.
#'
#' @param agg A \code{data.frame} object as created internally in \code{\link{aggregateTrack}}.
#' @return A numeric vector containing the linear distance between locations of adjacent visits,
#' whereby short-term visits are not counted as gaps and are considered as visits. Values for a following
#' move
#' are always assigned to the last value of a visit in \code{agg} as indicated by \code{agg$end}.
#'
#' @seealso \code{\link{aggregateTrack}}
#' @examples #
#' @keywords internal
#' @export
altitudinaldistancenextvisit_wis <- function(agg, aggcoords){

  # create a dummy vector with distances between adjacent locations
  gapsdistance <- rep(0, nrow(agg))

  # extract all location blocks
  locationblocks <- data.table::rleidv(agg, "location")

  # identify start and end values
  locationblocks <- data.table::rbindlist(tapply(seq_len(nrow(agg)), locationblocks, function(x){
    data.frame(start = x[1], end = x[length(x)])
  }))

  # discard the leading block (value cannot be assigned)
  locationblocks <- locationblocks[locationblocks$start != 1,]

  if(nrow(locationblocks) > 0){

    # get the altitudinal distance between points1 and points2
    locationblocks$distance <- agg$altitude[locationblocks$start] - agg$altitude[locationblocks$start-1]

    # insert values for the departure date prior the gap block
    gapsdistance[locationblocks$start - 1] <- locationblocks$distance

  }

  # return gapsdistance
  return(gapsdistance)

}

#' Computes Altitudinal Move Distances Not Ignoring Short-Term Visits
#'
#' \code{altitudinaldistancenextvisit_wos} computes for a \code{data.frame} as created
#' internally in \code{\link{aggregateTrack}} the linear altitudinal distance between locations
#' of adjacent campsites, whereby short-term visits are counted as gaps.
#'
#' @param agg A \code{data.frame} object as created internally in \code{\link{aggregateTrack}}.
#' @return A numeric vector containing the linear altitudinal distance between locations of adjacent campsites,
#' whereby short-term visits are counted as gaps. Values for a following
#' move
#' are always assigned to the last value of a visit in \code{agg} as indicated by \code{agg$end}.
#'
#' @seealso \code{\link{aggregateTrack}}
#' @examples #
#' @keywords internal
#' @export
altitudinaldistancenextvisit_wos <- function(agg, aggcoords){

  # extract all indices of campsite ends
  campsiteends <- which(agg$end & agg$campsite)

  # create a dummy vector with distances between adjacent locations
  gapsdistance <- rep(0, nrow(agg))

  # index for campsite visits
  campsiteindex <- which(agg$campsite)

  # extract all location blocks
  locationblocks <- data.table::rleidv(agg[campsiteindex,], "location")

  # identify start and end values
  locationblocks <- data.table::rbindlist(tapply(seq_len(nrow(agg))[campsiteindex], locationblocks, function(x){
    data.frame(start = x[1], end = x[length(x)])
  }))

  # discard the leading block (value cannot be assigned)
  locationblocks <- locationblocks[locationblocks$start != 1,]

  # discard all blocks with no previous campsite (value cannot be assigned)
  indexnopreviouscampsite <- which(sapply(locationblocks$start, function(x) any(campsiteends < x)) > 0)
  if(length(indexnopreviouscampsite) > 0){
    locationblocks <- locationblocks[sapply(locationblocks$start, function(x) any(campsiteends < x)),]
  }

  if(nrow(locationblocks) > 1){

    # get for each location block the nearest end of a campsite
    nearestvaluebeforegap <- sapply(locationblocks$start, function(x){

      # get all campsiteends < x
      campsiteends_before <- campsiteends[campsiteends < x]

      # get the nearest index
      campsiteends_before[which.min(abs(campsiteends_before - x))]

    })

    # get the altitudinal distance between points1 and points2
    locationblocks$distance <- agg$altitude[locationblocks$start] - agg$altitude[locationblocks$start-1]

    # insert values for the departure date prior the gap block
    gapsdistance[nearestvaluebeforegap] <- locationblocks$distance

  }

  # return gapsdistance
  return(gapsdistance)

}

#' Computes Move Altitudinal Distances Not Ignoring Short-Term Visits
#'
#' \code{altitudinaldistancenextvisit_wis_campsite} computes for a \code{data.frame} as created
#' internally in \code{\link{aggregateTrack}} the altitudinal linear distance between locations
#' of adjacent campsites along locations of short-term visits (if present). This is done
#' by summing all altitudinal distances covered between adjacent campsites as computed with
#' \code{\link{altitudinaldistancenextvisit_wis}}.
#'
#' @param agg A \code{data.frame} object as created internally in \code{\link{aggregateTrack}}.
#' @return A numeric vector containing the altitudinal linear distance between locations
#' of adjacent campsites along locations of short-term visits (if present). Values for a following
#' move
#' are always assigned to the last value of a campsite in \code{agg} as indicated by \code{agg$end}.
#'
#' @seealso \code{\link{aggregateTrack}}
#' @examples #
#' @keywords internal
#' @export
altitudinaldistancenextvisit_wis_campsite <- function(agg){

  # create a dummy vector with distances between adjacent locations
  gapsdistance <- rep(0, nrow(agg))

  # index for campsite visits
  campsiteindex <- which(agg$campsite)

  # extract all location blocks
  locationblocks <- data.table::rleidv(agg[campsiteindex,], "location")

  # identify start and end values
  locationblocks <- data.table::rbindlist(tapply(seq_len(nrow(agg))[campsiteindex], locationblocks, function(x){
    data.frame(start = x[1], end = x[length(x)])
  }))

  # discard the leading block (value cannot be assigned)
  locationblocks <- locationblocks[locationblocks$start != 1,]

  # change the indices so that they display interstices between campsites
  locationblocks <- data.frame(start = locationblocks$end[-nrow(locationblocks)], end = locationblocks$start[-1])

  if(nrow(locationblocks) > 0){

    # get the distance between campsites along short-term visit locations
    locationblocks$distance <- apply(locationblocks, 1, function(x) sum(agg$altitudinaldistancenextvisit_wis[x[1]:x[2]]))

    # insert values for the departure date prior the non-campsite block
    gapsdistance[locationblocks$start] <- locationblocks$distance

  }

  # return gapsdistance
  return(gapsdistance)

}
