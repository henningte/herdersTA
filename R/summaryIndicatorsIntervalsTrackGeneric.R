#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Computes summary indicators for GPS tracks.
#'
#' \code{summaryIndicatorsIntervalsTrackGeneric} computes various summary indicators
#' for a \code{\link[trajectories:Track-class]{Track}} object that has been processed
#' with \code{\link{reorganizeTrack}}, \code{\link{locationsTrack}} and
#' \code{\link{aggregateDailyLocationsTrack}}. Summary
#' indicators are computed for a temporal resolution of fixed ten-day intervals. In
#' contrast to
#' \code{summaryIndicatorsIntervalsTrack}, \code{summaryIndicatorsIntervalsTrackGeneric}
#' does not compute special summary indicators in a non-generic way, but
#' simply applies a user-defined function to a selection of variables of a
#' \code{\link[trajectories:Track-class]{Track}} object.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object that
#' has been processed as described in the description.
#' @param normalise A logical value indicating if the computed summary
#' indicators should be normalized relative to the duration of data values
#' (as specified by the variable \code{data$nogapduration} of \code{currenttrack})
#' for each fixed
#' ten-day interval (\code{normalize = TRUE}) or not (\code{normalize = FALSE}).
#' @param fun A function that takes a numeric vector as input and returns
#' a numeric value that is used in order to temporally aggregate the values.
#' @param what A numeric vector with indices of the variables in
#' \code{currenttrack@data} to summarise.
#' @return A \code{data.frame} object with with each row representing a time
#' interval as specified by the slot \code{data$id_timeinterval} of \code{currenttrack}
#' containing the following variables:
#' \describe{
#'   \item{\code{trackid}}{A character vector with the id (name) of
#'   \code{currenttrack}.}
#'   \item{\code{ftdi}}{A character vector with the first day of the respective
#'   fixed ten-day interval.}
#'   \item{\code{distance_tot}}{A numeric value representing the (normalized)
#'   total distance covered during a time interval [m].}
#'   \item{\code{altitude_tot}}{A numeric value representing the (normalized)
#'   total altitudinal distance covered (i.e. each metre covered in a vertical
#'   distance) during a time interval [m].}
#'   \item{\code{altitude_distance}}{A numeric value representing the (normalized)
#'   total altitudinal distance covered during a time interval [m] (i.e. the
#'   altitudinal difference between the first point in the time interval and the
#'   last point).}
#'   \item{\code{number_campsites}}{A numeric value representing the (normalized)
#'   total number of unique campsites (i.e. neglecting repeated visits or counting
#'   locations respectively) during a time interval.}
#' }
#' @seealso \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
summaryIndicatorsIntervalsTrackGeneric <- function(currenttrack,
                                    normalise = TRUE,
                                    fun = mean,
                                    what
                                    ){

  # check if currenttrack is of class Track
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack must be of class Track\n")
  }

  # assign each day in currenttrack$day to a ftdi
  ftdi <- assignFixedTenDayInterval(as.POSIXct(currenttrack$day, format = "%Y-%m-%d"), startnew = FALSE)
  ftdi <- ftdi[names(ftdi) %in% currenttrack$day]
  ftdi <- ftdi - min(ftdi) + 1

  # summarise the values for each ftdi
  summarisedvalues <-
    as.data.frame(cbind(names(ftdi[!duplicated(ftdi)]), tapply(seq_along(ftdi), ftdi, function(x){

    apply(matrix(currenttrack@data[x,what], ncol = length(what)), 2, function(y){
      fun(na.omit(y))
    })

  })), stringsAsFactors = FALSE)
  names(summarisedvalues) <- c("ftdi", names(currenttrack@data)[what])
  summarisedvalues[,-1] <- apply(matrix(summarisedvalues[,-1], ncol = length(what)), 1, function(x) as.numeric(as.character(x)))

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
