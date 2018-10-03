#'@importFrom Rdpack reprompt
#'@import trajectories
NULL

#' Computes the relative duration of gaps in GPS tracks.
#'
#' \code{gapProprtion} computes the relative duration of gaps [s] in a given
#' \code{\link[trajectories]{Track}} object (for an individual
#' \code{\link[trajectories]{Track}} object) relative to the total duration of
#' the intersection of the \code{\link[trajectories]{Track}} object and a
#' given time period, using the functions \code{\link{gapDuration}} and
#' \code{\link{timeActiveTrack}}.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object.
#' @param tstart A \code{POSIXct} object indicating the date and time of the start
#' of the target period. Default is the first value of the
#' \code{\link[trajectories]{Track}} object.
#' @param tend A \code{POSIXct} object indicating the date and time of the end of
#' the target period. Default is the last value of the
#' \code{\link[trajectories]{Track}} object.
#' @param threshold An integer value indicating the threshold for the time between
#' two data points [s] above which this is considered as a gap in the
#' \code{\link[trajectories]{Track}} object. Default is \code{threshold = 18000},
#' i.e. 5 hours. If \code{threshold = NULL}, the threshold value is calculated from
#' the data as \code{mean(duration) + tolerance * sd(duration)}.
#' @param tolerance An integer value indicating the tolerance of the threshold value
#' calculated from the data if \code{threshold = NULL}. Default is
#' \code{tolerance = 2}, i.e. 2 seconds.
#' @return A numeric value representing the sum of the duration of gaps within the
#' \code{\link[trajectories]{Track}} object [s] divided by the total duration of the
#' intersection between the \code{\link[trajectories]{Track}} object and the
#' specified time period.
#' @seealso \code{\link{findGaps}}, \code{\link{countGaps}},
#' \code{\link{gapDuration}}, \code{\link{timeActiveTrack}},
#' \code{\link{totalGapProportion}}.
#' @examples #
#' @export
gapProportion <- function(currenttrack,
                          tstart = min(index(currenttrack@time)),
                          tend = max(index(currenttrack@time)),
                          threshold = 18000,
                          tolerance = 2) {

  # get the relative duration of gaps in the Track object using gapDuration and timeActiveTrack
  gapDuration(currenttrack = currenttrack,
              tstart = tstart,
              tend = tend,
              threshold = threshold,
              tolerance = tolerance) /
  timeActiveTrack(currenttrack = currenttrack,
                  tstart = tstart,
                  tend = tend)

}
