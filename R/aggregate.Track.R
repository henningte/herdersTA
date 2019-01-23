#' @importFrom Rdpack reprompt
#' @import plyr join
#' @importFrom data.table rbindfill
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
  if(!(is.list(FUN) || names(FUN)[1] == "coords" || all(sapply(FUN, is.function)))){
    stop("FUN must be a list of functions with the first element named 'coords'\n")
  }

  # index of non-gap groups
  nongapgroupsindex <- which(x$group != 0)

  # collect any group (visit) related data
  groupdata <- data.table::rbindlist(tapply(seq_len(nrow(x@data))[nongapgroupsindex], x$group[nongapgroupsindex], function(y){

    data.frame(group = x$group[y[1]], location = x$location[y[1]], campsite = x$campsite[y[1]], norepeatedcampsitevisits = x$norepeatedcampsitevisits[y[1]], longitude = sp::coordinates(x@sp)[y[1],1], latitude = sp::coordinates(x@sp)[y[1],2], altitude = x$altitude[y[1]])

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

  # extract the coordinates
  aggcoords <- agg[,c("longitude", "latitude")]

  # fill gaps with dummy values (first from next visit for the previous gap and than from the previous visit for the next gab for the ending gap)
  aggcoords <- tidyr::fill(aggcoords, seq_len(ncol(aggcoords)), .direction = "up")
  aggcoords <- tidyr::fill(aggcoords, seq_len(ncol(aggcoords)), .direction = "down")

  # create the aggregated Track object
  trajectories::Track(spacetime::STIDF(sp = sp::SpatialPoints(coords = aggcoords, proj4string = CRS(proj4string(x))), time = agg$time, data = agg[,!(colnames(agg) %in% c("time", "longitude", "latitude"))], endTime = agg$time))

}
