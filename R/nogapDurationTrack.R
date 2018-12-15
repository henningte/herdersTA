#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Determines the duration of non-gaps in GPS tracks.
#'
#' \code{nogapDurationTrack} compute the temporal duration of not missing
#' values for time intervals of a \code{\link[trajectories:Track-class]{Track}} object
#' as returned by \code{removeDataTrack}, based on the classification of
#' missing values as gaps by \code{reorganizeTracks} and on the
#' classification of time intervals to consider during analyses by
#' \code{removeDataTrack}.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object with a
#' \code{boolean} column \code{gap} in \code{currenttrack@data} and an
#' \code{integer} column \code{timeinterval_id} in \code{currenttrack@data}. Data
#' values have to be regularly spaced (may be achieved for example with
#' \code{\link{reorganizeTracks}}).
#' @param timeinterval A numerical value reperesenting the duration of a
#' time interval represented by one data value of \code{currenttrack} [s].
#' @return A \code{\link[trajectories:Track-class]{Track}} object identical to
#' \code{currenttrack}, except for a new column
#' \code{currenttrack@data$duration_nogap}
#' representing the temporal duration of not missing values within the
#' corresponding time interval specified by
#' \code{currenttrack@data$id_timeinterval}.
#' @seealso \code{\link{identifyTimeIntervals}}, \code{\link{removeDataTrack}},
#' \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
nogapDurationTrack <- function(currenttrack, timeinterval = 30 * 60){

  # get duration of no gaps for each time interval defined by id_timeinterval
  duration_intervals <- sapply(unique(currenttrack@data$id_timeinterval), function(x){
    a <- length(currenttrack@data$id_timeinterval[which(currenttrack@data$gap == FALSE & currenttrack@data$id_timeinterval == x)]) * timeinterval
    names(a) <- x
    return(a)
  })

  # assign each value to the respective data values
  currenttrack@data$duration_nogap <- sapply(currenttrack@data$id_timeinterval, function(x){
    duration_intervals[which(names(duration_intervals) == x)]
  })

  # return result
  return(currenttrack)

}
