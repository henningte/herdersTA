#' Extracts a Vector of Matched Unique Values for Campsites.
#'
#' \code{summaryIndicatorsCampsitesTrack_arrivaltime} extracts for each
#' campsite the day when the household arrived. If this time point is
#' not sure, the function returns \code{NA}. The function can  be used in
#' combination with a
#' \code{\link[trajectories:Track-class]{Track}} object as returned by
#' \code{\link{aggregateDailyLocationsTrack}} for which unique campsite
#' visits are identified by a variable \code{aggregatedcmapsitevisits}.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object that
#' has been processed as described in the description. Additionally, the function
#' needs a logical variable indicating if the household arrived at a specific day
#' (\code{TRUE}) or not (\code{FALSE}) as \code{arrived}.
#' @return A \code{data.frame} object with a row for each campsite and two
#' columns:
#' \describe{
#'   \item{campsite}{An identifier for each unique campsite as provided by
#'   \code{currenttrack$aggregatedcampsitevists.}}
#'   \item{arrival}{A character vector representing the arrival days for each
#'   campsite.}
#' }
#' @seealso \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
summaryIndicatorsCampsitesTrack_arrivaltime <- function(currenttrack) {

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

      # extract the arrival time
      indexarrived <- which(currenttrack@data$arrived[index][x] == TRUE)
      if(length(indexarrived) != 0){
        currenttrack@data$day[index][x][indexarrived]
      }else{
        NA
      }

    })), stringsAsFactors = FALSE)
  names(summarisedvalues) <- c("campsite", "arrival")

  # return summarisedvalues
  return(summarisedvalues)

}
