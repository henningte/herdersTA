#' @importFrom trajectories TracksCollection
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel clusterExport makeCluster stopCluster parLapply
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
#' \code{\link[parallel:clusterApply]{clusterCall}}.
#' @return A \code{\link[trajectories:Track-class]{TracksCollection}} object with
#' a \code{\link[trajectories:Track-class]{Tracks}} object for each former
#' \code{\link[trajectories:Track-class]{Tracks}} object containing
#' the merged  \code{\link[trajectories:Track-class]{Track}} objects of the
#' input  \code{\link[trajectories:Track-class]{Tracks}} object as one
#' \code{\link[trajectories:Track-class]{Track}} object.
#' @seealso \code{\link{mergeTracks}}, \code{\link{reorganizeTracks}}.
#' @examples #
#' @export
mergeTracksCollection <- function(trackscollection, cores = 1, clcall = NULL){

    # set up cluster
    cl <- parallel::makeCluster(cores, outfile="", type = "PSOCK")
    doParallel::registerDoParallel(cl)
    if(is.null(clcall) == FALSE){
      parallel::clusterCall(cl, clcall)
    }
    on.exit(expr = parallel::stopCluster(cl))
    parallel::clusterCall(cl, function(){library("lubridate")})
    parallel::clusterCall(cl, function(){library("spacetime")})
    parallel::clusterCall(cl, function(){library("data.table")})
    parallel::clusterCall(cl, function(){library("trajectories")})
    parallel::clusterExport(cl = cl, varlist = list("exDataTracks",
                                                    "mergeTracks",
                                                    "trackscollection"),
                            envir = environment())

    # merge the Track objects of each Tracks object
    newtracks <- parallel::parLapply(cl, trackscollection@tracksCollection, fun = function(x){
      mergeTracks(currenttracks = x)
      })

  # convert newtracks to Tracks object and return the result
  newtracks <- trajectories::TracksCollection(newtracks)

  # set the Track names to the original names
  names(newtracks@tracksCollection) <- names(trackscollection@tracksCollection)

  # return the result
  return(newtracks)

}
