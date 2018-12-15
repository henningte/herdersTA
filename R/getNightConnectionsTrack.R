#'@importFrom Rdpack reprompt
#'@import trajectories
NULL

#' Extracts sub-daily connection values of GPS tracks.
#'
#' \code{getNightConnectionsTrack} subsets the values of the slot
#' \code{connections} of a \code{\link[trajectories:Track-class]{Track}} object by
#' extracting values that were
#' recorded within a certain time interval of a day (for example in order to
#' extract values that were recorded at night).
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object.
#' @param tstart A numeric value indicating the start of the time interval
#' [h, values between 0 and 24]. The default is \code{tstart = 16}.
#' @param tend A numeric value indicating the end of the time interval
#' [h, values between 0 and 24]. The default is \code{tstart = 20}.
#' @return The downsampled values of the slot \code{conections} of the
#' \code{\link[trajectories:Track-class]{Track}} object (containing only data points
#' within the specified time interval).
#' @seealso \code{\link{downByDrop}}, \code{\link{downByDropTrs}},
#' \code{\link{getNightTrs}}, \code{\link{daynightFixesTrack}}.
#' @examples #
#' @export
getNightConnectionsTrack <- function(currenttrack,
                                     tstart = 16,
                                     tend = 20){

  # get indices of fixes within time frame
  nightindexes <- which(hour(currenttrack@time) >= tstart & hour(currenttrack@time) < tend)

  # get connection having the nightly fixes as start
  nightconnections <- currenttrack@connections[nightindexes,]

  # extract only those connection having also a nightly fix as end (index +1)
  nightconnections[is.element(strtoi(row.names(nightconnections)) + 1, nightindexes),]

}
