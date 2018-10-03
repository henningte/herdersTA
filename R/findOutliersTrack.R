#'@importFrom Rdpack reprompt
#'@import trajectories
NULL

#' Finds outliers in GPS tracks.
#'
#' \code{findOutliersTrack} identifies outliers in
#' \code{\link[trajectories]{Track}} objects based on a threshold speed value
#' between succeeding measurements that is considered as outlier within the
#' respective scope.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object.
#' @param tstart A \code{POSIXct} object indicating the date and time of the start
#' of the target period. Default is the first value of the
#' \code{\link[trajectories]{Track}} object.
#' @param tend A \code{POSIXct} object indicating the date and time of the end of
#' the target period. Default is the last value of the
#' \code{\link[trajectories]{Track}} object.
#' @param threshold An integer value indicating the threshold for the speed between
#' two data points [m/s] above which this (connection) is considered as a
#' measurement error. Default is \code{threshold = 180}.
#' @return A \code{data.frame} object in the form of the connections slot of a
#' \code{\link[trajectories]{Track}} object containing connections with speed
#' values above \code{threshold}.
#' @seealso \code{\link{findGaps}}.
#' @examples #
#' @export
findOutliersTrack <- function(currenttrack,
                              tstart = min(index(currenttrack@time)),
                              tend = max(index(currenttrack@time)),
                              threshold = 180){

  # extract all connections with a speed > threshold
  connectionsub <- currenttrack@connections[index(currenttrack@time) >= tstart &
                             index(currenttrack@time) <= tend, ]

  na.omit(connectionsub[connectionsub$speed > threshold, ])

}
