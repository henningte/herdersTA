#'@importFrom trajectories Tracks
NULL

#' Cleans and regularises GPS tracks.
#'
#' \code{downByDropTrs} subsets the values of all \code{\link[trajectories:Track-class]{Track}}
#' objects of a given \code{\link[trajectories:Track-class]{Tracks}} object to a regular time
#' interval by using the function \code{\link{downByDrop}}.
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{Tracks}} object.
#' @param timeInterval An integer value indicating a time interval [s] as
#' target duration between data points of the \code{\link[trajectories:Track-class]{Track}}
#' object. Default is \code{timeInterval = 1770} (i.e. 29.5 minutes).
#' @return The downsampled \code{\link[trajectories:Track-class]{Tracks}} object.
#' @seealso \code{\link{downByDrop}}, \code{\link{getNightTrack}},
#' \code{\link{getNightTrs}}, \code{\link{daynightFixesTrack}},
#' \code{\link{daynightFixesTracks}},
#' \code{\link{getNightConnectionsTrack}}.
#' @examples #
#' @export
downByDropTrs <- function(currenttracks,
                          timeInterval = 1770) {

  # appply downByDrops to all Track objects of the Tracks object
  tracklist <- lapply(currenttracks@tracks, function(x) downByDrop(x))

  # remove list entries corresponding to empty Track objects
  tracklist <- tracklist[!sapply(tracklist, is.null)]

  # return the downsampled Tracks object
  if(length(tracklist) > 0){
    trajectories::Tracks(tracklist)
  }

}
