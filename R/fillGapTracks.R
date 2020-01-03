#' @importFrom trajectories Tracks
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel clusterExport makeCluster stopCluster parLapply
#' @importFrom raster crs
#' @importFrom sp proj4string
NULL

#' Imputes gaps in a \code{Tracks} object.
#'
#' \code{fillGapTracks} imputes missing values in all
#' \code{\link[trajectories:Track-class]{Track}} objects of a
#' \code{\link[trajectories:Track-class]{Tracks}} object. Gaps are filled
#' if their duration is \eqn{\le} a user specified duration threshold
#' and if the distance between the spatial position of the last data
#' value before the gap and the spatial position of the first data
#' value after the gap is \eqn{\le} a user specified distance threshold.
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{Tracks}} object with
#' \code{\link[trajectories:Track-class]{Track}} objects with a
#' boolean column \code{gap} in \code{track@data}. Data values
#' have to be regularly spaced (may be achieved for example with
#' \code{\link{reorganizeTracks}}).
#' @param maxduration A numerical value representing the maximum
#' allowed duration of a gap that is filled [s].
#' @param maxdistance A numerical value representing the maximum
#' allowed distance between the spatial position of the last data
#' value before a gap and the spatial position of the first data
#' value after a gap that is filled [m].
#' @param timeinterval A numerical value reperesenting the duration
#' of a time interval represented by one data value of \code{track} [s].
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel:clusterApply]{clusterCall}}.
#' @return The input \code{\link[trajectories:Track-class]{Tracks}} object with filled
#' gaps.
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{extractClustersBuffer}},
#' \code{\link{redefineIndices}},
#' \code{\link{fillGapTracks}}, \code{\link{locationsTrack}}.
#' @examples #
#' @export
fillGapTracks <- function(currenttracks,
                          maxduration,
                          maxdistance,
                          timeinterval,
                          cores = 1,
                          clcall = NULL) {

  # set up cluster
  cl <- parallel::makeCluster(cores, outfile="", type = "PSOCK")
  doParallel::registerDoParallel(cl)
  if(is.null(clcall) == F){
    parallel::clusterCall(cl, clcall)
  }
  parallel::clusterCall(cl, function(){library("spacetime")})
  parallel::clusterCall(cl, function(){library("trajectories")})
  parallel::clusterCall(cl, function(){library("raster")})
  parallel::clusterExport(cl = cl, varlist = list("currenttracks", "maxduration", "maxdistance", "timeinterval", "identifyBlocksVariable", "fillGapTrack"), envir=environment())
  on.exit(expr = parallel::stopCluster(cl))

  newcurrenttracks <- parallel::parLapply(cl, currenttracks@tracks, fun = function(x){

    newtracks1 <- fillGapTrack(currenttrack = x,
                               maxduration = maxduration,
                               maxdistance = maxdistance,
                               timeinterval = timeinterval)

    # set crs
    raster::crs(newtracks1@sp) <- sp::proj4string(currenttracks)

    return(newtracks1)

  })

  # convert to Tracks object
  newcurrenttracks <- trajectories::Tracks(newcurrenttracks)

  # return result
  return(newcurrenttracks)

}
