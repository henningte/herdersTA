#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Computes summary indicators for GPS tracks.
#'
#' \code{summaryIndicatorsIntervalsTrack_distance} computes the distance
#' a household covered during a fixed ten-day interval. Distances are
#' computed for each value within a fixed ten-day interval and additionally
#' for the distance between the first value of the following interval and the
#' last value of the target interval. The function requires a
#' \code{\link[trajectories:Track-class]{Track}} object as returned by
#' \code{\link{aggregateDailyLocationsTrack}}. Hence, only linear
#' distances between campsites are computed. If there is a gap with a fixed
#' tenday interval, it will be searched if there are values prior and after
#' the gap within this interval. If this is the case, the distance between
#' the values prior and after the gap will be summed because in this
#' case, it is sure that the distance has been covered within the
#' fixed ten-day interval. If this is not the case, the distance value of the
#' gap-value transition will be set to 0 which means that the will be ignored.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object that
#' has been processed as described in the description.
#' @param normalise A logical value indicating if the computed summary
#' indicators should be normalized relative to the duration of data values
#' (as specified by \code{currenttrack@data$nogapduration}) for each fixed
#' ten-day interval (\code{normalize = TRUE}) or not (\code{normalize = FALSE}).
#' @return A numeric vector with the (normalised) distance covered within
#' each fixed ten-day interval \code{currenttrack} covers.
#' @seealso \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
summaryIndicatorsIntervalsTrack_distance <- function(currenttrack,
                                                     normalise = FALSE
 ){

  # check if currenttrack is of class Track
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack must be of class Track\n")
  }

  # assign each day in currenttrack$day to a ftdi
  ftdi <- assignFixedTenDayInterval(as.POSIXct(currenttrack$day, format = "%Y-%m-%d"), startnew = FALSE)
  ftdi <- ftdi[names(ftdi) %in% currenttrack$day]
  ftdi <- ftdi - min(ftdi) + 1

  # exract the connections slot of currenttrack
  connections <- currenttrack@connections

  # add information on gaps to connections
  connections$gap <- ifelse(currenttrack$location[-1] == 0, TRUE, FALSE)

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
      currentgapblocks <- identifyBlocksVariable(currenttrack = currentconnections, variable = "gap", value = TRUE)
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
      apply(matrix(currentconnections$distance, ncol = 1), 2, function(y){
        sum(na.omit(y))
      })

    })), stringsAsFactors = FALSE)
  names(summarisedvalues) <- c("ftdi", "distance")
  summarisedvalues$altitude <- as.numeric(as.character(summarisedvalues$distance))

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
