#' @importFrom stats na.omit
NULL

#' Computes summary indicators for GPS tracks.
#'
#' \code{summaryIndicatorsIntervalsTrack_timesmoved} computes the number
#' of campsite moves during a fixed ten-day interval. For each fixed
#' ten-day interval, the maximum number of arrivals OR departures OR
#' distinct campsite visits (i.e. at different locations) is considered
#' as the number of moves. This is done because exact arrival or departure
#' times can only be given if there is no gap (that is too long) before
#' a data value at a new location. The function requires a
#' \code{\link[trajectories:Track-class]{Track}} object as returned by
#' \code{\link{aggregateDailyLocationsTrack}}. Hence, only linear
#' altitudeinal differences between campsites are computed.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object that
#' has been processed as described in the description.
#' @param normalise A logical value indicating if the computed summary
#' indicators should be normalized relative to the duration of data values
#' (as specified by \code{currenttrack@data$nogapduration}) for each fixed
#' ten-day interval (\code{normalize = TRUE}) or not (\code{normalize = FALSE}).
#' @return A numeric vector with the (normalised) total altitudinal height
#' differences covered within each fixed ten-day interval \code{currenttrack}
#' covers.
#' @seealso \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
summaryIndicatorsIntervalsTrack_timesmoved <- function(currenttrack,
                                                       normalise = FALSE) {

  # check if currenttrack is of class Track
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack must be of class Track\n")
  }

  # assign each day in currenttrack$day to a ftdi
  ftdi <- assignFixedTenDayInterval(as.POSIXct(currenttrack$day, format = "%Y-%m-%d"),
                                    startnew = FALSE)
  ftdi <- ftdi[names(ftdi) %in% currenttrack$day]
  ftdi <- ftdi - min(ftdi) + 1

  # summarise the values for each ftdi
  summarisedvalues <-
    as.data.frame(cbind(names(ftdi[!duplicated(ftdi)]), tapply(seq_along(ftdi), ftdi, function(x){

      # compute the number of arrivals
      noarrivals <- sum(stats::na.omit(as.numeric(as.logical(currenttrack@data$arrived[x]))))

      # compute the number of departures
      nodepartures <- sum(na.omit(as.numeric(as.logical(currenttrack@data$left[x]))))

      # compute the number of different campsite locations
      locations <- unique(currenttrack@data$location[x])
      locations <- locations[locations != 0]
      nodifferentlocations <- length(locations) - 1

      # return the max of noarrivals OR nodepartures OR nodifferentlocations
      max(noarrivals, nodepartures, nodifferentlocations)

    })), stringsAsFactors = FALSE)
  names(summarisedvalues) <- c("ftdi", "altitude")
  summarisedvalues$altitude <- as.numeric(as.character(summarisedvalues$altitude))

  # normalise
  if(normalise == TRUE){

    # compute the proportion of nogaps for each ftdi
    nogapsproportionftdi <- tapply(seq_along(ftdi), ftdi, function(x){

      apply(matrix(ifelse(currenttrack@data$nogapsproportion[x] > 0, 1, 0), ncol = 1), 2, function(y){
        sum(y)/length(y)
      })

    })

    # normalise the extracted values
    summarisedvalues[,-1] <- apply(matrix(summarisedvalues[,-1], ncol = length(what)), 2, function(x) sapply(seq_along(nogapsproportionftdi), function(y) if(nogapsproportionftdi[y] == 0){NA}else{x[y]/nogapsproportionftdi[y]}))

  }

  # return summarisedvalues
  return(summarisedvalues)

}
