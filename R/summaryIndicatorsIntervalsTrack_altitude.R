#' @importFrom stats na.omit
NULL

#' Computes summary indicators for GPS tracks.
#'
#' \code{summaryIndicatorsIntervalsTrack_altitude} computes the sum of
#' the height differences (i.e. all covered height meters)
#' a household covered during a fixed ten-day interval. Height
#' distances are
#' computed for each value within a fixed ten-day interval and additionally
#'  between the first value of the following interval and the
#' last value of the target interval. The function requires a
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
#' @return A numeric vector with the (normalised) altitudinal height
#' differences covered within each fixed ten-day interval \code{currenttrack}
#' covers.
#' @seealso \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
summaryIndicatorsIntervalsTrack_altitude <- function(currenttrack,
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

  # exract the connections slot of currenttrack
  connections <- currenttrack@connections

  # add informatio on gaps to connections
  connections$gap <- ifelse(currenttrack$location[-1] == 0, TRUE, FALSE)

  # compute altitudinal differences between all adjacent data values
  connections$altitude <- currenttrack$altitude[-1] - currenttrack$altitude[-length(currenttrack$altitude)]

  # flag connection values as not reliable that connect a gap value with a nogap value
  connections$notreliable <- sapply(seq_len(nrow(currenttrack@data))[-1], function(x){
    ifelse((currenttrack@data$location[x] != 0 && currenttrack@data$location[x-1] == 0) || (currenttrack@data$location[x] == 0 && currenttrack@data$location[x-1] != 0), TRUE, FALSE)
  })

  # summarise the values for each ftdi
  summarisedvalues <-
    as.data.frame(cbind(names(ftdi[!duplicated(ftdi)]), tapply(seq_along(ftdi), ftdi, function(x){

      # extract the respective values of connections$distance
      currentconnections <- as.data.frame(connections[c(x, x[length(x)]+1),])

      # identify block of gaps in connection
      currentgapblocks <- identifyBlocksVariable(currenttrack = currentconnections,
                                                 variable = "gap",
                                                 value = TRUE)
      if(!is.null(currentgapblocks)){

        # remove blocks at the boundary of the tdi
        currentgapblocks <- currentgapblocks[which(currentgapblocks[1,1] != 1 & currentgapblocks[nrow(currentgapblocks), 2] != nrow(currentconnections)), ,drop = FALSE]

        if(length(currentgapblocks) > 0){

          # extract the row indices and convert to a vector
          currentgapblocks <- do.call("c", lapply(seq_len(nrow(currentgapblocks)), function(y){
            currentgapblocks[y,1]:(currentgapblocks[y,2] + 1)
          }))

          # set the respective values of currentconnections$notreliable to FALSE
          currentconnections$notreliable[currentgapblocks] <- FALSE

        }

      }

      # set non-reliable altitude distance values to 0
      currentconnections$altitude[currentconnections$notreliable == TRUE] <- 0

      # sum the values
      apply(matrix(currentconnections$altitude, ncol = 1), 2, function(y){
        sum(stats::na.omit(y))
      })

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
