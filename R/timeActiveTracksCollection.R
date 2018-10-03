#'@importFrom Rdpack reprompt
#'@import trajectories
NULL

#' Analyses the temporal coverage of GPS tracks.
#'
#' \code{timeActiveTrackCollection} computes the time [s] during which position data
#' was collected in all \code{\link[trajectories]{Tracks}} objects of a specified
#' \code{\link[trajectories]{TracksCollection}} object within a given time period.
#'
#' @param trcollection A \code{\link[trajectories]{TracksCollection}} object.
#' @param tstart A \code{POSIXct} object indicating the date and time of the start
#' of the target period.
#' @param tend A \code{POSIXct} object indicating the date and time of the end of
#' the target period.
#' @return A numerical value representing the proportional coverage of the
#' intersection between each \code{\link[trajectories]{Tracks}} object and the
#' specified time period relative to the total duration of the specified time
#' period.
#' @seealso \code{\link{timeActiveTrack}}, \code{\link{timeActiveTracks}}.
#' @examples #
#' @export
timeActiveTracksCollection <- function(trcollection, tstart, tend) {

  # apply timeActiveTracks to all Tracks objects
  trcollection@tracksCollectionData$tactive <- unlist(
    lapply(
      trcollection@tracksCollection,
      timeActiveTracks,
      tstart = tstart,
      tend = tend
    )
  )

  # compute and return the intersection proportion relative to the duration of the time period
  trcollection@tracksCollectionData$tactive/as.integer(difftime(tend, tstart, units = "sec")) * 100

}
