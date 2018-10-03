#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import spacetime
#' @import doParallel
#' @import raster
NULL

#' Imputes gaps in a \code{\link[trajectories]{Tracks}} object.
#'
#' \code{fillGapTracks} imputes missing values in all
#' \code{\link[trajectories]{Track}} objects of a
#' \code{\link[trajectories]{Tracks}} object. Gaps are filled
#' if their duration is $\le$ a user specified duration threshold
#' and if the distance between the spatial position of the last data
#' value before the gap and the spatial position of the first data
#' value after the gap is $\le$ a user specified distance threshold.
#'
#' @param currenttracks A \code{\link[trajectories]{Tracks}} object with
#' \code{\link[trajectories]{Track}} objects with a
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
#' \code{\link[parallel]{clusterCall}}.
#' @return The input \code{\link[trajectories]{Tracks}} object with filled
#' gaps.
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{extractClutersBuffer}},
#' \code{\link{redefineIndices}},
#' \code{\link{fillGapTracks}}, \code{\link{locationsTrack}}.
#' @examples #
#' @export
fillGapTracks <- function(currenttracks,
                          maxduration,
                          maxdistance,
                          timeinterval,
                          cores = 1,
                          clcall = NULL){

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(is.null(clcall) == F){
    clusterCall(cl, clcall)
  }
  clusterCall(cl, function(){library("spacetime")})
  clusterCall(cl, function(){library("trajectories")})
  clusterCall(cl, function(){library("raster")})
  clusterExport(cl = cl, varlist = list("currenttracks", "maxduration", "maxdistance", "timeinterval", "identifyBlocksVariable", "fillGapTrack"), envir=environment())

  newcurrenttracks <- parLapply(cl, currenttracks@tracks, fun = function(x){

    newtracks1 <- fillGapTrack(currenttrack = x,
                               maxduration = maxduration,
                               maxdistance = maxdistance,
                               timeinterval = timeinterval)

    # set crs
    crs(newtracks1@sp) <- proj4string(currenttracks)

    return(newtracks1)

  })

  # convert to Tracks object
  newcurrenttracks <- Tracks(newcurrenttracks)

  # stop cluster
  stopCluster(cl)

  # return result
  return(newcurrenttracks)

}
