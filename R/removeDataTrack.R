#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Classifies GPS track parts as evaluatable.
#'
#' \code{removeDataTrack} determine if data values of a
#' \code{\link[trajectories]{Track}} object for specific time
#' intervals should be classified as usable (i.e. they should
#' not be removed in following analyses) or not, based on a
#' user specified threshold value of the proportion of missing
#' values within a month. Uses the function
#' \code{\link{identifyTimeIntervals}} in order to classify data
#' values according to time intervals.

#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object with a
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
#' @return A \code{\link[trajectories]{Track}} object identical with
#' \code{currenttrack}, except for a new column \code{currenttrack@data$remove}
#' indicating if a data value should be included in following analyses (
#' \code{currenttrack@data$remove == FALSE}) or not (
#' \code{currenttrack@data$remove == TRUE}), based on the specifications of the user,
#' a new column \code{currenttrack@data$id_timeinterval} representing the id of the
#' respective time interval specified by \code{timeinterval} and a new column
#' \code{currenttrack@data$proportion_gaps} representing the temporal proportion of
#' missing values within a time interval specified by
#' \code{currenttrack@data$id_timeinterval}.
#' @seealso \code{\link{identifyTimeIntervals}}, \code{\link{removeDataTracks}},
#' \code{\link{nogapDurationTrack}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
removeDataTrack <- function(currenttrack, timeinterval = "month", threshold = 40){

  # assign data values to time intervals
  timeinterval <- identifyTimeIntervals(currenttrack, timeinterval)
  currenttrack@data$id_timeinterval <- timeinterval

  # classify data values as removed or not
  removed <- rep(FALSE, nrow(currenttrack@data))
  prop_gaps_ges <- NULL
  for(timeinterval_i in unique(timeinterval)){

    # get the proportion of gaps
    prop_gaps <- length(which(currenttrack@data$gap[which(timeinterval == timeinterval_i)] == TRUE))/length(currenttrack@data$gap[which(timeinterval == timeinterval_i)]) * 100
    prop_gaps_ges <- c(prop_gaps_ges, rep(prop_gaps, length(which(timeinterval == timeinterval_i))))

    # classify data values as removed or not
    if(prop_gaps > threshold){
      removed[which(timeinterval == timeinterval_i)] <- TRUE
    }

  }

  # add removed to currenttrack@data
  currenttrack@data$remove <- removed

  # add proportion_gaps to currenttrack@data
  currenttrack@data$proportion_gaps <- prop_gaps_ges

  # return result
  return(currenttrack)

}
