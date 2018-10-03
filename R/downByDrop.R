#'@importFrom Rdpack reprompt
#'@import trajectories
NULL

#' Cleans and regularises GPS tracks.
#'
#' \code{downByDrop} subsets the values of a \code{\link[trajectories]{Track}}
#' object to a regular time interval by cleaning the measured values. The first
#' value of the \code{\link[trajectories]{Track}} object represents the starting
#' point of the time point specification according to the target time interval.
#' The last entry of a sequence of positioning measurements of the GPS tracker
#' (in "spy" mode) is retained assuming that the assessed position data is the
#' most accurate within the corresponding sequence. Points of the
#' \code{\link[trajectories]{Track}} object are only returned if there are at
#' least two data values with a time interval as long as the specified time
#' interval in the \code{\link[trajectories]{Track}} object.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object.
#' @param timeInterval An integer value indicating a time interval [s] as
#' target duration between data points of the \code{\link[trajectories]{Track}}
#' object. Default is \code{timeInterval = 1770} (i.e. 29.5 minutes).
#' @return The downsampled \code{\link[trajectories]{Track}} object.
#' @seealso \code{\link{downByDropTrs}}, \code{\link{getNightTrack}},
#' \code{\link{getNightTrs}}, \code{\link{daynightFixesTrack}},
#' \code{\link{daynightFixesTracks}},
#' \code{\link{getNightConnectionsTrack}}.
#' @examples #
#' @export
downByDrop <- function(currenttrack, timeInterval = 1770){

  # get the number of values in the Track objetc
  j <- length(currenttrack@time)

  # initialise a vector to store indices of values to discard
  dropindexes <- integer()

  # go through all fixes starting with the one before the last one and check the time interval to the following fix in the track
  for(i in (length(currenttrack@time)-1):1){

    #if interval is below timeInterval (1770 secs), current fix will be dropped, "j" remains the following fix in the track
    if (difftime(currenttrack@time[j], currenttrack@time[i], units = "secs") < timeInterval){

      dropindexes <- c(dropindexes, i)

    }else{

      # else: current fix will not be dropped, but set as the new following fix "j"
      j <- i

    }
  }

  # build new Track from data at all indexes not in 'dropindexes', but only if there are at least two of them
  if(length(dropindexes) < length(currenttrack@time) - 1){
    downsampled <-
      Track(
        STIDF(
          sp = currenttrack@sp[-dropindexes],
          time = currenttrack@time[-dropindexes],
          endTime = currenttrack@endTime[-dropindexes],
          data = currenttrack@data[-dropindexes, ]
        )
      )
    return(downsampled)
  }

}
