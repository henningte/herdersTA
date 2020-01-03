#' Analyses the temporal coverage of GPS tracks.
#'
#' \code{timeActiveTracks} computes the time [s] during which position data was
#' collected in all \code{\link[trajectories:Track-class]{Track}} objects of a specified
#' \code{\link[trajectories:Track-class]{Tracks}} object within a given time period. The
#' coverage of all \code{\link[trajectories:Track-class]{Track}} objects is summed.
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{Tracks}} object.
#' @param tstart A \code{POSIXct} object indicating the date and time of the start
#' of the target period.
#' @param tend A \code{POSIXct} object indicating the date and time of the end of
#' the target period.
#' @return A numerical value representing the summed values of
#' \code{timeActiveTrack} for all \code{\link[trajectories:Track-class]{Track}} objects of the
#' specified \code{\link[trajectories:Track-class]{Tracks}} object. This value corresponds to
#' the number of seconds the \code{\link[trajectories:Track-class]{Tracks}} object and the time
#' period intersect.
#' @seealso \code{\link{timeActiveTrack}}, \code{\link{timeActiveTracksCollection}}.
#' @examples #
#' @export
timeActiveTracks <- function(currenttracks,
                             tstart,
                             tend) {

  # sum all results of timeActiveTrack for all Track objects
  sum(unlist(
    lapply(
      currenttracks@tracks,
      timeActiveTrack,
      tstart = tstart,
      tend = tend
    )
  ))

}
