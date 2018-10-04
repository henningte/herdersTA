#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import doParallel
#' @import raster
NULL

#' Determines the duration of gaps in GPS tracks.
#'
#' \code{nogapDurationTracks} compute the temporal duration of not missing
#' values for time intervals of all \code{\link[trajectories]{Track}} objects
#' in a \code{\link[trajectories]{Tracks}} object
#' as returned by \code{removeDataTracks}, based on the classification of
#' missing values as gaps by \code{reorganizeTracks} and on the
#' classification of time intervals to consider during analyses by
#' \code{removeDataTracks}.
#'
#' @param currenttracks A \code{\link[trajectories]{Tracks}} object with
#' \code{\link[trajectories]{Track}} objects with a
#' \code{boolean} column \code{gap} in \code{currenttrack@data} and an
#' \code{integer} column \code{timeinterval_id} in \code{currenttrack@data}. Data
#' values have to be regularly spaced (may be achieved for example with
#' \code{\link{reorganizeTracks}}).
#' @param timeinterval A numerical value reperesenting the duration of a
#' time interval represented by one data value of \code{currenttrack} [s].
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel]{clusterCall}}.
#' @return A \code{\link[trajectories]{Tracks}} object identical to
#' \code{currenttracks}, except for a new column
#' \code{currenttrack@data$duration_nogap}
#' representing the temporal duration of of not missing values within the
#' corresponding time interval specified by
#' \code{currenttrack@data$id_timeinterval} for
#' each \code{\link[trajectories]{Track}} object (\code{currenttrack}).
#' @seealso \code{\link{identifyTimeIntervals}}, \code{\link{removeDataTrack}},
#' \code{\link{removeDataTracks}}, \code{\link{nogapDurationTrack}}.
#' @examples #
#' @export
nogapDurationTracks <- function(currenttracks, timeinterval = 30 * 60, cores = 1, clcall = NULL){

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(is.null(clcall) == FALSE){
    clusterCall(cl, clcall)
  }
  clusterCall(cl, function(){library("raster")})
  clusterCall(cl, function(){library("spacetime")})
  clusterCall(cl, function(){library("trajectories")})
  clusterExport(cl = cl, varlist = list("currenttracks", "timeinterval", "gapDurationTrack"), envir=environment())

  newcurrenttracks <- parLapply(cl, currenttracks@tracks, fun = function(x){

    newcurrenttracks1 <- gapDurationTrack(track = x, timeinterval = timeinterval)

    return(newcurrenttracks1)

  })

  # convert to Tracks object
  newcurrenttracks <- Tracks(newcurrenttracks)

  # stop cluster
  stopCluster(cl)

  # return result
  return(newcurrenttracks)

}
