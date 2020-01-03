#' Analyses the temporal coverage of GPS tracks.
#'
#' \code{timeActiveTrack} computes the time [s] during which position data was
#' collected in a given \code{\link[trajectories:Track-class]{Track}} object (for an
#' individual \code{\link[trajectories:Track-class]{Track}} object) within a given time period.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object.
#' @param tstart A \code{POSIXct} object indicating the date and time of the start
#' of the target period.
#' @param tend A \code{POSIXct} object indicating the date and time of the end of
#' the target period.
#' @return A numerical value:
#' \itemize{
#'   \item 0 If the track does not cover the time period.
#'   \item 1 If the track touches the time period (i.e. the maximum time value of
#'   the track equals the start value of the time period or the minimum time
#'   value of the track equals the end value of the time period).
#'   \item The number of seconds the time period and the track intersect in any
#'   other case.
#' }
#' @seealso \code{\link{timeActiveTracks}}, \code{\link{timeActiveTracksCollection}}.
#' @examples #
#' @export
timeActiveTrack <- function(currenttrack,
                            tstart,
                            tend) {

  # return 0 if no intersection and touching of the track and the time period
  if (max(index(currenttrack@time)) < tstart | min(index(currenttrack@time)) > tend){
    0
  }
  # return 1 if currenttrack touches the time frame (e.g. last fix of track equals start of time frame)
  else if(max(index(currenttrack@time)) == tstart | min(index(currenttrack@time)) == tend) {
    1
  }
  # return the number of seconds in case of intersections
  else{
    as.double(difftime(
      max(index(currenttrack@time)[index(currenttrack@time) <= tend]),
      min(index(currenttrack@time)[index(currenttrack@time) >= tstart]),
      units = "sec"))
  }

}
