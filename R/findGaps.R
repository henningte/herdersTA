#' @importFrom stats na.omit
NULL

#' Finds gaps in GPS tracks.
#'
#' \code{findGaps} finds gaps in a given \code{\link[trajectories:Track-class]{Track}} object
#' (for an individual \code{\link[trajectories:Track-class]{Track}} object) within a given time
#' period, based on the duration between succeeding measurements and a threshold
#' value.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object.
#' @param tstart A \code{POSIXct} object indicating the date and time of the start
#' of the target period. Default is the first value of the
#' \code{\link[trajectories:Track-class]{Track}} object.
#' @param tend A \code{POSIXct} object indicating the date and time of the end of
#' the target period. Default is the last value of the
#' \code{\link[trajectories:Track-class]{Track}} object.
#' @param threshold An integer value indicating the threshold for the time between
#' two data points [s] above which this is considered as a gap in the
#' \code{\link[trajectories:Track-class]{Track}} object. Default is \code{threshold = 18000},
#' i.e. 5 hours. If \code{threshold = NULL}, the threshold value is calculated from
#' the data as \code{mean(duration) + tolerance * sd(duration)}.
#' @param tolerance An integer value indicating the tolerance of the threshold value
#' calculated from the data if \code{threshold = NULL}. Default is
#' \code{tolerance = 2}, i.e. 2 seconds.
#' @return A \code{data.frame} object in the form of the connections slot of a
#' \code{\link[trajectories:Track-class]{Track}} object containing gaps between data points
#' as entries.
#' @seealso \code{\link{countGaps}}, \code{\link{gapDuration}},
#' \code{\link{gapProportion}}.
#' @examples #
#' @export
findGaps <- function(currenttrack,
                     tstart = min(index(currenttrack@time)),
                     tend = max(index(currenttrack@time)),
                     threshold = 18000,
                     tolerance = 2) {


  if (is.null(threshold) == TRUE){

    # compute a threshold value from the data if not specified
    gap_thresh <- (mean(currenttrack@connections$duration) + tolerance * sd(currenttrack@connections$duration))

    # extract the gaps
    connectionsub <- currenttrack@connections[index(currenttrack@time) >= tstart &
                                              index(currenttrack@time) <= tend,]

    stats::na.omit(connectionsub[connectionsub$duration > gap_thresh,])

  }else{

    # extract the gaps
    connectionsub <- currenttrack@connections[index(currenttrack@time) >= tstart &
                                                index(currenttrack@time) <= tend,]

    stats::na.omit(connectionsub[connectionsub$duration > threshold,])

  }

}
