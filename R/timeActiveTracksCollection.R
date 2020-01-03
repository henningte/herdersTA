#' Analyses the temporal coverage of GPS tracks.
#'
#' \code{timeActiveTracksCollection} computes the time [s] during which position data
#' was collected in all \code{\link[trajectories:Track-class]{Tracks}} objects of a specified
#' \code{\link[trajectories:Track-class]{TracksCollection}} object within a given time period.
#'
#' @param trcollection A \code{\link[trajectories:Track-class]{TracksCollection}} object.
#' @param tstart A \code{POSIXct} object indicating the date and time of the start
#' of the target period.
#' @param tend A \code{POSIXct} object indicating the date and time of the end of
#' the target period.
#' @return A numerical value representing the proportional coverage of the
#' intersection between each \code{\link[trajectories:Track-class]{Tracks}} object and the
#' specified time period relative to the total duration of the specified time
#' period.
#' @seealso \code{\link{timeActiveTrack}}, \code{\link{timeActiveTracks}}.
#' @examples #
#' @export
timeActiveTracksCollection <- function(trcollection,
                                       tstart,
                                       tend) {

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
