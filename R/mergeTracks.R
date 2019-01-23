#'@importFrom Rdpack reprompt
#'@import spacetime
#'@import lubridate
NULL

#' Merges \code{\link[trajectories:Track-class]{Track}} objects of a \code{\link[trajectories:Track-class]{Tracks}} object.
#'
#' \code{mergeTracks} merges the \code{\link[trajectories:Track-class]{Track}}
#' objects of a \code{\link[trajectories:Track-class]{Tracks}} objects into a
#' singular \code{\link[trajectories:Track-class]{Track}} object.
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{Tracks}} object.
#' @return A  \code{\link[trajectories:Track-class]{Track}} object containing
#' the merged  \code{\link[trajectories:Track-class]{Track}} objects of the
#' input  \code{\link[trajectories:Track-class]{Tracks}} object.
#' @seealso \code{\link{mergeTracksCollection}},
#' \code{\link{reorganizeTracks}}.
#' @examples #
#' @export
mergeTracks <- function(currenttracks){

  # extract data
  df <- exDataTracks(currenttracks = currenttracks)

  # extract spatial data
  trsSP <- as(as(currenttracks, "SpatialLines"), "SpatialPoints")

  # extract time information
  time <- do.call("c", lapply(currenttracks@tracks, function(x) as.POSIXct(x@time)))

  # create and return the Track object
  Track(STIDF(sp = trsSP, time = time , data = df, endTime = time))

}
