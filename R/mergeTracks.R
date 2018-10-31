#'@importFrom Rdpack reprompt
#'@import spacetime
#'@import lubridate
NULL

#' Merges \code{\link[trajectories]{Track}} objects of a \code{\link[trajectories]{Tracks}} object.
#'
#' \code{mergeTracks} merges the \code{\link[trajectories]{Track}}
#' objects of a \code{\link[trajectories]{Tracks}} objects into a
#' singular \code{\link[trajectories]{Track}} object.
#'
#' @param currenttracks A \code{\link[trajectories]{Tracks}} object.
#' @return A  \code{\link[trajectories]{Track}} object containing
#' the merged  \code{\link[trajectories]{Track}} objects of the
#' input  \code{\link[trajectories]{Tracks}} object.
#' @seealso \code{\link{mergeTracksCollection}},
#' \code{\link{reorganizeTracks}}.
#' @examples #
#' @export
mergeTracks <- function(currenttracks){

  # extract data
  df <- exDataTracks(currenttracks = currenttracks)

  # extract spatial data
  trsSP <- as(as(currenttracks, "SpatialLines"), "SpatialPoints")

  if(length(as.POSIXct(df$time)) == 2){
    time1 <- as.POSIXct(df$time)
    time2 <- as.POSIXct(df$time)
  }else{
    time1 <- as.POSIXct(df$time)[-length(df$time)]
    time2 <- as.POSIXct(df$time)[-1]
  }

  # create and return the Track object
  Track(STIDF(sp = trsSP, time = time1 , data = df, endTime = time2))

}
