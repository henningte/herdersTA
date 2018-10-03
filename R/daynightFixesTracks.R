#'@importFrom Rdpack reprompt
#'@import trajectories
#'@import sp
NULL

#' Classifies sub-daily values of GPS tracks.
#'
#' \code{daynightFixesTracks} classifies the values of all
#' \code{\link[trajectories]{Track}} object of a given
#' \code{\link[trajectories]{Tracks}} objectin two classes on a
#' sub-daily basis by defining threshold hour values (for example
#' in order to classify values as recorded during daytime and
#' nighttime).
#'
#' @param currenttracks A \code{\link[trajectories]{Tracks}} object.
#' @param tstart A numeric value indicating the start of the time interval
#' [h, values between 0 and 24]. \code{tstart} has to be smaller than
#' \code{tend}.
#' @param tend A numeric value indicating the end of the time interval
#' [h, values between 0 and 24]. \code{tend} has to be larger than
#' \code{tstart}.
#' @return a \code{\link[sp]{SpatialPointsDataFrame}} containing all data
#' of the slot \code{data} of the \code{\link[trajectories]{Track}}
#' objects of the input \code{\link[trajectories]{Tracks}}
#' object and a column \code{night} indicating if a data point is within
#' the specified time interval (\code{night = 1}) or not
#' (\code{night = 0}).
#' @seealso \code{\link{downByDrop}}, \code{\link{downByDropTrs}},
#' \code{\link{getNightTrack}}, \code{\link{getNightTrs}},
#' \code{\link{dayNightFixesTrack}},
#' \code{\link{getNightConnectionsTrack}}.
#' @examples #
#' @export
daynightFixesTracks <- function(currenttracks,
                                tstart = 16,
                                tend = 20) {

  # apply dayNightFixesTrack on all Track objects and merge the results
  do.call(
    rbind,
    lapply(
      currenttracks@tracks,
      daynightFixesTrack,
      tstart = tstart,
      tend = tend
    )
  )

}
