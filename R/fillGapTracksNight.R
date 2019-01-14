#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import spacetime
#' @import doParallel
#' @import raster
NULL

#' Imputes gaps in a \code{\link[trajectories]{Tracks}} object.
#'
#' \code{fillGapTracksNight} imputes missing values in all
#' \code{\link[trajectories:Track-class]{Track}} objects of a
#' \code{\link[trajectories:Track-class]{Tracks}} object. Gaps are filled
#' if their duration is \eqn{\le} a user specified duration threshold
#' and if the distance between the spatial position of the last data
#' value before the gap and the spatial position of the first data
#' value after the gap is \eqn{\le} a user specified distance threshold.
#' In contrast to \code{\link{fillGapTrack}}, \code{fillGapTrackNight}
#' considers only values in a specified time interval of the day (e.g.
#' during night).
#'
#' It has to be paid
#' attention to the fact that \code{maxduration} should be adapted to
#' the time interval specified by \code{night}. For example, if one specifies
#' the time interval between 0 and 4 o' clock as night and wants to allow
#' a \code{maxduration} of four days (assuming that there may be gaps for four
#' following nights, but if there is no gap in the fifth night, then the gap is
#' filled), then one must specifiy \code{maxduration} as
#' \code{4*24*60*60 + (24 - nightduration)*60*60}, whereby \code{nightduration}
#' is the duration of the time interval specified as night in hours.
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
#' @param night An integer vector with two elements:
#' \enumerate{
#'   \item The first element specifies the start hour of the night, e.g. \code{0}
#'   for 0 o'clock.
#'   \item The first element specifies the start hour of the night, e.g. \code{4}
#'   for 4 o'clock.
#' }
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
fillGapTracksNight <- function(currenttracks,
                          maxduration,
                          maxdistance,
                          timeinterval,
                          night = c(16, 20),
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
  clusterExport(cl = cl, varlist = list("currenttracks", "maxduration", "maxdistance", "timeinterval", "night", "identifyBlocksVariable", "fillGapTrack"), envir=environment())

  newcurrenttracks <- parLapply(cl, currenttracks@tracks, fun = function(x){

    newtracks1 <- fillGapTrackNight(currenttrack = x,
                               maxduration = maxduration,
                               maxdistance = maxdistance,
                               timeinterval = timeinterval,
                               night = night)

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
