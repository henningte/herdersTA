#' @importFrom stats na.omit
NULL

#' Computes summary indicators for GPS tracks.
#'
#' \code{summaryIndicatorsIntervalsTrack_distancebyclass} computes the distance
#' a household covered during a fixed ten-day interval and for each class
#' defined by an additional variable (e.g. distance covered within certain
#' landcover classes). Distances are
#' computed for each value within a fixed ten-day interval and additionally
#' for the distance between the first value of the following interval and the
#' last value of the target interval. The function requires a
#' \code{\link[trajectories:Track-class]{Track}} object as returned by
#' \code{\link{aggregateDailyLocationsTrack}}. Hence, only linear
#' distances between campsites are computed.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object that
#' has been processed as described in the description.
#' @param normalise A logical value indicating if the computed summary
#' indicators should be normalized relative to the duration of data values
#' (as specified by \code{currenttrack@data$nogapduration}) for each fixed
#' ten-day interval (\code{normalize = TRUE}) or not (\code{normalize = FALSE}).
#' @param classvariable A character value indicating a variable name of
#' \code{currenttrack@data}. For each unique value of \code{classvariable}
#' and each fixed ten-day interval, summary values will be computed.
#' @return A \code{data.frame} object with a row for each fixed ten-day interval
#' , a column \code{ftdi} indicating the first day of the respective fixed ten-day
#' interval and a column for each level in \code{classvariable} with the distance
#' value for the respective fixed ten-day interval and class level.
#' @seealso \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
summaryIndicatorsIntervalsTrack_distancebyclass <- function(currenttrack,
                                                            normalise = FALSE,
                                                            classvariable) {

  # check if currenttrack is of class Track
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack must be of class Track\n")
  }

  # check if class variable is specified correctly
  if(!(inherits(classvariable, "character") || length(classvariable) == 1)){
    stop("classvariable must be a character value\n")
  }

  # assign each day in currenttrack$day to a ftdi
  ftdi <- assignFixedTenDayInterval(as.POSIXct(currenttrack$day, format = "%Y-%m-%d"),
                                    startnew = FALSE)
  ftdi <- ftdi[names(ftdi) %in% currenttrack$day]
  ftdi <- ftdi - min(ftdi) + 1

  # summarise the values for each ftdi
  summarisedvalues <-
    as.data.frame(cbind(names(ftdi[!duplicated(ftdi)]), do.call("rbind.fill", tapply(seq_along(ftdi), ftdi, function(x){

      # get the class indices
      index <- c(x[1]-1, x)
      index <- index[index != 0]
      classindices <- tapply(index, currenttrack@data[c(x, x[length(x)]+1), names(currenttrack@data) == classvariable], function(y){
        y
      }, simplify = FALSE)

      summarisedvalues <- sapply(classindices, function(y){
        apply(matrix(currenttrack@connections$distance[y], ncol = 1), 2, function(z){
          sum(stats::na.omit(z))
        })
      })
      names(summarisedvalues) <- names(classindices)
      summarisedvalues <- as.data.frame(matrix(summarisedvalues, nrow = 1),
                                        stringsAsFactors = FALSE)
      colnames(summarisedvalues) <- names(classindices)

      return(summarisedvalues)

    }))), stringsAsFactors = FALSE)
  names(summarisedvalues)[1] <- c("ftdi")
  names(summarisedvalues)[-1] <- paste0("distance_", names(summarisedvalues)[-1])
  summarisedvalues[,-1] <- apply(summarisedvalues[,-1], 2, function(x) as.numeric(as.character(x)))

  # remove the column for gaps
  summarisedvalues[,2] <- NULL

  # normalise
  if(normalise == TRUE){

    # compute the proportion of nogaps for each ftdi
    nogapsproportionftdi <- tapply(seq_along(ftdi), ftdi, function(x){

      apply(matrix(ifelse(currenttrack@data$nogapsproportion[x] > 0, 1, 0), ncol = 1), 2, function(y){
        sum(y)/length(y)
      })

    })

    # normalise the extracted values
    summarisedvalues[,-1] <- apply(matrix(summarisedvalues[,-1], ncol = ncol(summarisedvalues)-1), 2, function(x) sapply(seq_along(nogapsproportionftdi), function(y) if(nogapsproportionftdi[y] == 0){NA}else{x[y]/nogapsproportionftdi[y]}))

  }

  # return summarisedvalues
  return(summarisedvalues)

}
