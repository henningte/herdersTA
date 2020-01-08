#' @importFrom sp spDists
NULL

#' Imputes gaps in a \code{Track} object.
#'
#' \code{fillGapTrackNight} imputes missing values in a
#' \code{\link[trajectories:Track-class]{Track}} object. Gaps are filled
#' if their duration is \eqn{\le} a user specified duration threshold
#' and if the distance between the spatial position of the last data
#' value before the gap and the spatial position of the first data
#' value after the gap is \eqn{\le} a user specified distance threshold.
#'
#' It has to be paid
#' attention to the fact that \code{maxduration} should be adapted to
#' the time interval specified by \code{night}. For example, if one specifies
#' the time interval between 0 and 4 o' clock as night and wants to allow
#' a \code{maxduration} of four days (assuming that there may be gaps for four
#' following nights, but if there is no gap in the fifth night, then the gap is
#' filled), then one must specifiy \code{maxduration} as
#' \code{4*24*60*60 + (24 - nightduration)*60*60}, whereby \code{nightduration}
#' is the duration of the time interval specified as night in hours.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object with a
#' boolean column \code{gap} in \code{currenttrack@data}. Data values
#' have to be regularly spaced (may be achieved for example with
#' \code{\link{reorganizeTracks}}).
#' @param maxduration A numerical value representing the maximum
#' allowed duration of a gap that is filled [s]. It has to be paid
#' attention to the fact that \code{maxduration} should be adapted to
#' the time interval specified by \code{night}. See the details section.
#' @param maxdistance A numerical value representing the maximum
#' allowed distance between the spatial position of the last data
#' value before a gap and the spatial position of the first data
#' value after a gap that is filled [m].
#' @param night An integer vector with two elements:
#' \enumerate{
#'   \item The first element specifies the start hour of the night, e.g. \code{0}
#'   for 0 o'clock.
#'   \item The first element specifies the start hour of the night, e.g. \code{4}
#'   for 4 o'clock.
#' }
#' @return The input \code{\link[trajectories:Track-class]{Track}} object with
#' the variable \code{gap} set to \code{FALSE} for filled gap values. Additionally,
#' the output has a new variable \code{filled} indicating if a gap was filled
#' (\code{filled = TRUE}) or not (\code{filled = FALSE})
#' @seealso
#' \code{\link{fillGapTracksNight}}.
#' @examples #
#' @export
fillGapTrackNight <- function(currenttrack,
                              maxduration,
                              maxdistance,
                              night = c(16, 20)){

  # classify data values as night or day values
  currentnighttrack <- classifyNightTrack(currenttrack,
                                          night = c(16, 20))

  # get the row indices of night values in currentnighttrack that are no gaps
  currentnighttrackindex <- which(attr(currentnighttrack, "night") == TRUE & currentnighttrack@data$gap == FALSE)

  # extract blocks of gap values
  gaps <- identifyBlocksVariable(currenttrack = currenttrack@data,
                                 variable = "gap",
                                 value = TRUE)

  # discard leading and ending gaps (cannot be filled)
  gaps <- gaps[gaps$start != 1 & gaps$end != nrow(currenttrack@data),]

  # discard all gaps with gaps$start < min(currentnighttrackindex) or gaps$end > max(currentnighttrackindex) (cannote be filled based on the night values)
  gaps <- gaps[!(gaps$start < currentnighttrackindex[1] | gaps$end > currentnighttrackindex[length(currentnighttrackindex)]), ]

  if(nrow(gaps) > 0){

    # get for each gap the nearest night value before and after the gap
    nearestvaluebeforegap <- sapply(gaps$start, function(x){

      # get all currentnighttrackindex < x
      currentnighttrackindex_before <- currentnighttrackindex[currentnighttrackindex < x]

      # get the nearest index
      currentnighttrackindex_before[which.min(abs(currentnighttrackindex_before - x))]

    })
    nearestvalueaftergap <- sapply(gaps$end, function(x){

      # get all currentnighttrackindex > x
      currentnighttrackindex_after <- currentnighttrackindex[currentnighttrackindex > x]

      # get the nearest index
      currentnighttrackindex_after[which.min(abs(currentnighttrackindex_after - x))]

    })
    gaps$nearestvaluebeforegap <- nearestvaluebeforegap
    gaps$nearestvalueaftergap <- nearestvalueaftergap

    # extract the SpatialPoints for all values prior and after gaps
    beforegaps <- currenttrack@sp[gaps$nearestvaluebeforegap,]
    aftergaps <- currenttrack@sp[gaps$nearestvalueaftergap,]

    # compute the distance between points before and after gaps
    gaps$gapdistance <- sp::spDists(x = beforegaps,
                                    y = aftergaps,
                                    longlat = FALSE,
                                    diagonal = TRUE)

    # extract the time for all values prior and after gaps
    gaptime <- data.frame(before = as.POSIXct(currenttrack@time[gaps$start-1,]),
                          after = as.POSIXct(currenttrack@time[gaps$end+1,]))

    # compute the time difference (duration of the gap)
    gaps$gapduration <- difftime(time1 = gaptime$after,
                                 time2 = gaptime$before,
                                 units = "secs")

    # check which gaps to fill
    gaps$tofill <- ifelse(gaps$gapduration <= maxduration & gaps$gapdistance <= maxdistance, TRUE, FALSE)

    # get indices of gaps to fill in the original data set
    gapstofill <- unlist(apply(gaps[gaps$tofill,], 1, function(x) x[1]:x[2]))

  }else{
    gapstofill <- logical()
  }

  # define a new variable filled
  currenttrack@data$filled <- FALSE
  currenttrack@data$filled[gapstofill] <- TRUE

  # set curenttrack$gap to FALSE for filled gaps
  currenttrack@data$gap[gapstofill] <- FALSE

  # return currenttrack
  return(currenttrack)

}
