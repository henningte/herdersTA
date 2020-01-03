#' @importFrom sp CRS proj4string
#' @importFrom trajectories Track
#' @importFrom spacetime STIDF
NULL

#' Extracts Linear Distances Between Campsite Locations.
#'
#' \code{summaryIndicatorsCampsitesTrack_distance} extracts for each
#' campsite the linear distance to the following campsite. For the first
#' campsite the value is set to \code{NA} in order to indicate that the
#' distance to the previous campsite is unknown. The
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
#'   \item{distance}{A numeric vector representing the linear distance
#'   between the current campsite (indicated by \code{campsite}) and the
#'   following campsite.}
#' }
#' @seealso \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
summaryIndicatorsCampsitesTrack_distance <- function(currenttrack) {

  # check if currenttrack is of class Track
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack must be of class Track\n")
  }

  # define an index in order to exclude gaps and duplicated campsites
  index <- which(currenttrack$location != 0 & !duplicated(currenttrack$aggregatedcampsitevisits))

  if(length(index) > 1){

    # construct a new Track object omitting gaps and with one value per campsite
    currenttrack <- trajectories::Track(track = spacetime::STIDF(
      sp = SpatialPoints(coords = data.frame(lon = currenttrack$longitude,
                                             lat = currenttrack$latitude)[index,],
                         proj4string = sp::CRS(sp::proj4string(currenttrack@sp))),
      time = currenttrack@time[index],
      endTime = currenttrack@time[index],
      data = currenttrack@data[index,])
    )

    # extract the distance
    dist <- currenttrack@connections$distance

    # extract the unique campsites
    uniquecampsites <- currenttrack$aggregatedcampsitevisits
  }else{

    if(length(index) == 0){

      return(NULL)

    }else{
      # extract the unique campsites
      uniquecampsites <- currenttrack$aggregatedcampsitevisits[index]
    }

    # there is at most one campsite
    dist <- NULL

  }

  # extract the connections
  summarisedvalues <- data.frame(uniquecampsites,
                                 distance = c(NA, dist))

  # return summarisedvalues
  return(summarisedvalues)

}
