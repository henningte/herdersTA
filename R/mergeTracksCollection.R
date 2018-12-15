#'@importFrom Rdpack reprompt
#'@import spacetime
#'@import lubridate
#'@import doParallel
#'@import foreach
NULL

#' Merges \code{\link[trajectories:Track-class]{Track}} objects of a \code{\link[trajectories:Track-class]{Tracks}} objects.
#'
#' \code{mergeTracksCollection} merges the \code{\link[trajectories:Track-class]{Track}}
#' objects for each \code{\link[trajectories:Track-class]{Tracks}} object of a
#' \code{\link[trajectories:Track-class]{TracksCollection}} into a
#' singular \code{\link[trajectories:Track-class]{Track}} object.
#'
#' @param currenttrackscollection A
#' \code{\link[trajectories:Track-class]{TracksCollection}} object.
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel]{clusterCall}}.
#' @return A \code{\link[trajectories:Track-class]{Tracks}} object with
#' a \code{\link[trajectories:Track-class]{Track}} object for each former
#' \code{\link[trajectories:Track-class]{Tracks}} object containing
#' the merged  \code{\link[trajectories:Track-class]{Track}} objects of the
#' input  \code{\link[trajectories:Track-class]{Tracks}} object.
#' @seealso \code{\link{mergeTracks}}, \code{\link{reorganizeTracks}}.
#' @examples #
#' @export
mergeTracksCollection <- function(trackscollection, cores = 1, clcall = NULL){

    # set up cluster
    cl <- makeCluster(cores, outfile="", type = "PSOCK")
    registerDoParallel(cl)
    if(is.null(clcall) == FALSE){
      clusterCall(cl, clcall)
    }
    clusterCall(cl, function(){library("lubridate")})
    clusterCall(cl, function(){library("spacetime")})
    clusterExport(cl = cl, varlist = list("exDataTracks", "mergeTracks", "trackscollection"), envir=environment())

    # merge the Track objects of each Tracks object
    newtracks <- parLapply(cl, trackscollection@tracksCollection, fun = function(x){mergeTracks(currenttracks = x)})

    # stop cluster
    stopCluster(cl)


  # convert newtracks to Tracks object and return the result
  newtracks <- Tracks(newtracks)

  # set the Track names to the original names
  names(newtracks@tracks) <- names(trackscollection@tracksCollection)

  # return the result
  return(newtracks)

}
