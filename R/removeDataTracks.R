#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import doParallel
#' @import spacetime
NULL

#' Classifies GPS track parts as evaluatable.
#'
#' \code{removeDataTracks} determine for all
#' \code{\link[trajectories]{Track}} objects of a given
#' \code{\link[trajectories]{Tracks}} object if data values of the
#' \code{\link[trajectories]{Track}} objects for specific time
#' intervals should be classified as usable (i.e. they should
#' not be removed in following analyses) or not, based on a
#' user specified threshold value of the proportion of missing
#' values within a month. Uses the function
#' \code{\link{identifyTimeIntervals}} in order to classify data
#' values according to time intervals.

#'
#' @param currenttracks A \code{\link[trajectories]{Tracks}} object with
#' \code{\link[trajectories]{Track}} objects (\code{currenttrack}) with a
#' \code{boolean} column \code{gap} in \code{currenttrack@data} and a
#' \code{POSIXct} column \code{time} in \code{currenttrack@data}. Data
#' values have to be regularly spaced (may be achieved for example with
#' \code{\link{reorganizeTracks}}).
#' @param timeinterval One of \code{"year"}, \code{"month"},
#' \code{"tendayinterval"}, \code{"day"} or \code{"all"}. If
#' \code{timeinterval = "year"}, data values will be classified on a
#' yearly resolution. If \code{timeinterval = "month"}, data values
#' will be classified on a monthly resolution. If
#' \code{timeinterval = "tendayinterval"}, data values will be classified
#' on a ten-day interval resolution (i.e. assigned to fixed ten-day intervals).
#' Each month has three ten-day intervals
#' with the last having varying lengths of 11 to 8 days (depending on the
#' month and the occurance of leap years). If \code{timeinterval = "day"},
#' data values will be classified on a daily resolution. All time intervals
#' that are not completely covered by \code{currenttrack} (i.e. potentially
#' at the start and end of \code{currenttrack}) are handled as if they would
#' represent the whole specified time interval. If \code{timeintervall = "all"},
#' all data values will be assigned to one class. Default is
#' \code{timeinterval = "month"}.
#' @param threshold A numerical value representing the maximum proportion
#' of data values within a timeinterval that is allowed to represent gaps
#' (\code{currenttrack@data$gap == TRUE}) in order to \emph{not} discard all data
#' values for the corresponding month [%]. Default is \code{threshold = 40}.
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel]{clusterCall}}.
#' @return A \code{\link[trajectories]{Tracks}} object identical with
#' \code{currenttracks}, except for a new column \code{currenttrack@data$remove}
#' in each \code{\link[trajectories]{Track}} (\code{currenttrack}) object
#' indicating if a data value should be included in following analyses (
#' \code{currenttrack@data$remove == FALSE}) or not (
#' \code{currenttrack@data$remove == TRUE}), based on the specifications of the user,
#' a new column \code{currenttrack@data$id_timeinterval} representing the id of the
#' respective time interval specified by \code{timeinterval} and a new column
#' \code{currenttrack@data$proportion_gaps} representing the temporal proportion of
#' missing values within a time interval specified by
#' \code{currenttrack@data$id_timeinterval}.
#' @seealso \code{\link{identifyTimeIntervals}}, \code{\link{removeDataTrack}},
#' \code{\link{nogapDurationTrack}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
removeDataTracks <- function(currenttracks,
                             timeinterval = "month",
                             threshold = 40,
                             cores = 1,
                             clcall = NULL){

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(is.null(clcall) == FALSE){
    clusterCall(cl, clcall)
  }
  clusterCall(cl, function(){library("raster")})
  clusterCall(cl, function(){library("spacetime")})
  clusterCall(cl, function(){library("trajectories")})
  clusterExport(cl = cl, varlist = list("currenttracks", "timeinterval", "threshold", "removeDataTrack", "identifyTimeIntervals"), envir=environment())

  newcurrenttracks <- parLapply(cl, currenttracks@tracks, fun = function(x){

    new.tracks1 <- removeDataTrack(track = x, timeinterval = timeinterval, threshold = threshold)

    return(new.tracks1)

  })

  # convert to Tracks object
  newcurrenttracks <- Tracks(newcurrenttracks)

  # stop cluster
  stopCluster(cl)

  # return result
  return(newcurrenttracks)

}
