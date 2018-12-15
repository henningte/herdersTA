#'@importFrom Rdpack reprompt
#'@import trajectories
NULL

#' Extracts sub-daily values of GPS tracks.
#'
#' \code{getNightTrs} subsets the values of all \code{\link[trajectories:Track-class]{Track}}
#' objects of a given \code{\link[trajectories:Track-class]{Tracks}}
#' object by extracting values that were recorded within a certain time interval
#' of a day (for example in order to extract values that were recorded at night).
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{Tracks}} object.
#' @param tstart A numeric value indicating the start of the time interval
#' [h, values between 0 and 24]. The default is \code{tstart = 16}.
#' @param tend A numeric value indicating the end of the time interval
#' [h, values between 0 and 24]. The default is \code{tstart = 20}.
#' @return The downsampled \code{\link[trajectories:Track-class]{Tracks}} object
#' (containing only data points within the specified time interval).
#' @seealso \code{\link{downByDrop}}, \code{\link{downByDropTrs}},
#' \code{\link{getNightTrack}}, \code{\link{daynightFixesTrack}},
#' \code{\link{daynightFixesTracks}},
#' \code{\link{getNightConnectionsTrack}}.
#' @examples #
#' @export
getNightTrs <- function(currenttrs, tstart = 16, tend = 20){

  # apply getNightTrack to all Track objects of the Tracks object
  tracklist <- lapply(currenttracks@tracks,
                      getNightTrack,
                      tstart=tstart,
                      tend =tend)

  # remove list entries corresponding to empty Track objects
  tracklist <- tracklist[!sapply(tracklist, is.null)]

  # return the downsampled Tracks object
  if(length(tracklist) > 0){
    Tracks(tracklist)
  }
}
