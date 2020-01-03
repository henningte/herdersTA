#' @importFrom parallel clusterExport makeCluster stopCluster parLapply
#' @importFrom doParallel registerDoParallel
#' @importFrom trajectories Tracks
NULL

#' Determines the duration of non-gaps in GPS tracks.
#'
#' \code{nogapDurationTracks} compute the temporal duration of not missing
#' values for time intervals of all \code{\link[trajectories:Track-class]{Track}} objects
#' in a \code{\link[trajectories:Track-class]{Tracks}} object
#' as returned by \code{removeDataTracks}, based on the classification of
#' missing values as gaps by \code{reorganizeTracks} and on the
#' classification of time intervals to consider during analyses by
#' \code{removeDataTracks}.
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{Tracks}} object with
#' \code{\link[trajectories:Track-class]{Track}} objects with a
#' \code{boolean} column \code{gap} in \code{currenttrack@data} and an
#' \code{integer} column \code{timeinterval_id} in \code{currenttrack@data}. Data
#' values have to be regularly spaced (may be achieved for example with
#' \code{\link{reorganizeTracks}}).
#' @param timeinterval A numerical value reperesenting the duration of a
#' time interval represented by one data value of \code{currenttrack} [s].
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel:clusterApply]{clusterCall}}.
#' @return A \code{\link[trajectories:Track-class]{Tracks}} object identical to
#' \code{currenttracks}, except for a new column
#' \code{currenttrack@data$duration_nogap}
#' representing the temporal duration of not missing values within the
#' corresponding time interval specified by
#' \code{currenttrack@data$id_timeinterval} for
#' each \code{\link[trajectories:Track-class]{Track}} object (\code{currenttrack}).
#' @seealso \code{\link{identifyTimeIntervals}}, \code{\link{removeDataTrack}},
#' \code{\link{removeDataTracks}}, \code{\link{nogapDurationTrack}}.
#' @examples #
#' @export
nogapDurationTracks <- function(currenttracks,
                                timeinterval = 30 * 60,
                                cores = 1,
                                clcall = NULL) {

  # set up cluster
  cl <- parallel::makeCluster(cores, outfile="", type = "PSOCK")
  doParallel::registerDoParallel(cl)
  if(is.null(clcall) == FALSE){
    parallel::clusterCall(cl, clcall)
  }
  parallel::clusterCall(cl, function(){library("raster")})
  parallel::clusterCall(cl, function(){library("spacetime")})
  parallel::clusterCall(cl, function(){library("trajectories")})
  parallel::clusterExport(cl = cl,
                          varlist = list("currenttracks",
                                         "timeinterval",
                                         "nogapDurationTrack"),
                          envir=environment())
  on.exit(expr = parallel::stopCluster(cl))

  newcurrenttracks <- parallel::parLapply(cl, currenttracks@tracks, fun = function(x){
    nogapDurationTrack(currenttrack = x, timeinterval = timeinterval)
  })

  # convert to Tracks object
  trajectories::Tracks(newcurrenttracks)

}
