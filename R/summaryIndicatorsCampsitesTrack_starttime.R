#' Extracts a Vector of Matched Unique Values for Campsites.
#'
#' \code{summaryIndicatorsCampsitesTrack_starttime} extracts for each
#' campsite the day for which the first data values are available. The
#' function can  be used in combination with a
#' \code{\link[trajectories:Track-class]{Track}} object as returned by
#' \code{\link{aggregateDailyLocationsTrack}} for which unique campsite
#' visits are identified by a variable \code{aggregatedcmapsitevisits}.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object that
#' has been processed as described in the description.
#' @return A \code{data.frame} object with a row for each campsite and two
#' columns:
#' \describe{
#'   \item{campsite}{An identifier for each unique campsite as provided by
#'   \code{currenttrack$aggregatedcampsitevists.}}
#'   \item{start}{A character vector representing the days for which the
#'   first data values are available for each campsite.}
#' }
#' @seealso \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
summaryIndicatorsCampsitesTrack_starttime <- function(currenttrack
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

      # extract the start time
      currenttrack@data$day[index][x][1]

    })), stringsAsFactors = FALSE)
  names(summarisedvalues) <- c("campsite", "starttime")

  # return summarisedvalues
  return(summarisedvalues)

}
