#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Computes summary indicators for GPS tracks.
#'
#' \code{summaryIndicatorsCampsitesTrackGeneric} can be used to compute various
#' summary indicators for a \code{\link[trajectories:Track-class]{Track}} object
#' that has been processed with \code{\link{reorganizeTrack}},
#' \code{\link{locationsTrack}} and \code{\link{aggregateDailyLocationsTrack}}.
#' Summary indicators are computed for each campsite.
#' \code{summaryIndicatorsIntervalsTrackGeneric}
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
#' @return A \code{data.frame} object with with each row representing a campsite
#' and two columns
#' \describe{
#'   \item{\code{campsite}}{A character vector indicating the campsite (combination
#'   of the location number and the number of repeated visits).}
#'   \item{\code{value}}{A vector with the aggregated values.}
#'
#' }
#' @seealso \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
summaryIndicatorsCampsitesTrackGeneric <- function(currenttrack,
                                                   normalise = TRUE,
                                                   fun = mean,
                                                   what
){

  # check if currenttrack is of class Track
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack must be of class Track\n")
  }

  # define an index in order to exclude gaps
  index <- which(currenttrack$location != 0)

  # assign each day to a campsite
  campsites <- currenttrack$aggregatedcampsitevisits[index]
  names(campsites) <- currenttrack$day[index]

  # define a vector with unique campsites
  uniquecampsites <- campsites[!duplicated(campsites)]

  # summarise the values for each ftdi
  summarisedvalues <-
    as.data.frame(cbind(campsites[!duplicated(campsites)], lapply(seq_along(uniquecampsites), function(x){

      # get the indices in campsites corresponding to the current campsite
      x <- which(campsites == uniquecampsites[[x]])

      apply(matrix(currenttrack@data[index,][x,what], ncol = length(what)), 2, function(y){
        fun(na.omit(y))
      })

    })), stringsAsFactors = FALSE)
  names(summarisedvalues) <- c("campsite", names(currenttrack@data)[what])
  summarisedvalues[,-1] <- apply(matrix(summarisedvalues[,-1], ncol = length(what)), 1, function(x) as.numeric(as.character(x)))

  # normalise
  if(normalise == TRUE){

    # compute the proportion of nogaps for each ftdi
    nogapsproportioncampsites <- tapply(seq_along(campsites), campsites, function(x){

      apply(matrix(ifelse(currenttrack@data$nogapsproportion[index][x] > 0, 1, 0), ncol = 1), 2, function(y){
        sum(y)/length(y)
      })

    })

    # normalise the extracted values
    summarisedvalues[,-1] <- apply(matrix(summarisedvalues[,-1], ncol = length(what)), 2, function(x) sapply(seq_along(nogapsproportioncampsites), function(y) if(nogapsproportioncampsites[y] == 0){NA}else{x[y]/nogapsproportioncampsites[y]}))

  }

  # return summarisedvalues
  return(summarisedvalues)

}
