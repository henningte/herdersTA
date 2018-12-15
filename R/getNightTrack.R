#'@importFrom Rdpack reprompt
#'@import trajectories
NULL

#' Extracts sub-daily values of GPS tracks.
#'
#' \code{getNightTrack} subsets the values of a \code{\link[trajectories:Track-class]{Track}}
#' object by extracting values that were recorded within a certain time interval
#' of a day (for example in order to extract values that were recorded at night).
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object.
#' @param tstart A numeric value indicating the start of the time interval
#' [h, values between 0 and 24]. The default is \code{tstart = 16}.
#' @param tend A numeric value indicating the end of the time interval
#' [h, values between 0 and 24]. The default is \code{tstart = 20}.
#' @return The downsampled \code{\link[trajectories:Track-class]{Track}} object
#' (containing only data points within the specified time interval).
#' @seealso \code{\link{downByDrop}}, \code{\link{downByDropTrs}},
#' \code{\link{getNightTrs}}, \code{\link{daynightFixesTrack}},
#' \code{\link{daynightFixesTracks}},
#' \code{\link{getNightConnectionsTrack}}.
#' @examples #
#' @export
getNightTrack <- function(currenttrack, tstart = 16, tend = 20){

  # get indices of fixes within the specified daily time interval
  nightindexes <- which(hour(currenttrack@time) >= tstart & hour(currenttrack@time) < tend)

  #build new Track from data at all indexes in 'nightindexes', but only if there are at least two of them
  if(length(nightindexes) > 1){
      Track(
        STIDF(
          sp = currenttrack@sp[nightindexes],
          time = currenttrack@time[nightindexes],
          endTime = currenttrack@endTime[nightindexes],
          data = currenttrack@data[nightindexes, ]
        )
      )
  }

}
