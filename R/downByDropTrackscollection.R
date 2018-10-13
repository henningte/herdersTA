#'@importFrom Rdpack reprompt
#'@import trajectories
NULL

#' Cleans and regularises GPS tracks.
#'
#' \code{downByDropTrackscollection} subsets the values of all
#' \code{\link[trajectories]{Track}} objects of all
#' \code{\link[trajectories]{Tracks}} objects of a given
#' \code{\link[trajectories]{TracksCollection}} object to a regular time
#' interval by using the function \code{\link{downByDrop}}.
#'
#' @param currenttrackscollection A
#' \code{\link[trajectories]{TracksCollection}} object.
#' @param timeInterval An integer value indicating a time interval [s] as
#' target duration between data points of the \code{\link[trajectories]{Track}}
#' object. Default is \code{timeInterval = 1770} (i.e. 29.5 minutes).
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel]{clusterCall}}.
#' @return The downsampled \code{\link[trajectories]{TracksCollection}}
#' object.
#' @seealso \code{\link{downByDrop}}, \code{\link{getNightTrack}},
#' \code{\link{getNightTrs}}, \code{\link{daynightFixesTrack}},
#' \code{\link{daynightFixesTracks}},
#' \code{\link{getNightConnectionsTrack}}.
#' @examples #
#' @export
downByDropTrackscollection <- function(currenttrackscollection, timeInterval = 1770, cores = 1, clcall = NULL){

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(is.null(clcall) == F){
    clusterCall(cl, clcall)
  }
  clusterCall(cl, function(){library("trajectories")})
  clusterExport(cl = cl, varlist = list("currenttrackscollection", "timeInterval",
                                        "downByDrop", "downByDropTrs"), envir = environment())

  parLapply(cl, currenttrackscollection@tracksCollection, function(x){

    downByDropTrs(currenttracks = x, timeInterval = timeInterval)

  })

  # stop th cluster
  stopCluster(cl)

}
