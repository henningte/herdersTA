#' @importFrom trajectories Tracks TracksCollection
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel clusterExport makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
NULL

#' Imputes gaps in a \code{Tracks} object.
#'
#' \code{fillGapTracksNight} imputes missing values in all
#' \code{\link[trajectories:Track-class]{Track}} objects of the
#' \code{\link[trajectories:Track-class]{Tracks}} objects of a
#' \code{\link[trajectories:Track-class]{TracksCollection}}. Gaps are filled
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
#' @param currenttracks A \code{\link[trajectories:Track-class]{TracksCollection}} object with
#' \code{\link[trajectories:Track-class]{Tracks}} objects with each haveing only one
#' \code{\link[trajectories:Track-class]{Track}} object with a
#' boolean column \code{gap} in \code{track@data}. Data values
#' have to be regularly spaced (may be achieved for example with
#' \code{\link{reorganizeTracks}}).
#' @param maxduration A numerical value representing the maximum
#' allowed duration of a gap that is filled [s].
#' @param maxdistance A numerical value representing the maximum
#' allowed distance between the spatial position of the last data
#' value before a gap and the spatial position of the first data
#' value after a gap that is filled [m].
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
#' \code{\link[parallel:clusterApply]{clusterCall}}.
#' @return The input \code{\link[trajectories:Track-class]{Tracks}} object with filled
#' gaps.
#' @seealso \code{\link{reorganizeTracks}}.
#' @examples #
#' @export
fillGapTracksNight <- function(currenttracks,
                               maxduration,
                               maxdistance,
                               night = c(16, 20),
                               cores = 1,
                               clcall = NULL) {

  # extract the names
  currenttracksnames <- names(currenttracks@tracksCollection)

  # set up cluster
  cl <- parallel::makeCluster(cores, outfile="", type = "PSOCK")
  doParallel::registerDoParallel(cl)
  if(is.null(clcall) == F){
    parallel::clusterCall(cl, clcall)
  }
  on.exit(expr = parallel::stopCluster(cl))

  # fill gaps
  newcurrenttracks <- trajectories::TracksCollection(
    foreach::foreach(x = currenttracks@tracksCollection,
                     .packages = c("trajectories", "sp", "lubridate"),
                     .export = c("fillGapTrackNight"))%dopar%{

                       newtracks1 <- trajectories::Tracks(list(fillGapTrackNight(currenttrack = x@tracks[[1]],
                                                                                 maxduration = maxduration,
                                                                                 maxdistance = maxdistance,
                                                                                 night = night)))

  })

  # restore the names
  names(newcurrenttracks@tracksCollection) <- currenttracksnames

  # return result
  return(newcurrenttracks)

}
