#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Computes the mean distance between campsite locations of a Track object in the order of movement.
#'
#' \code{averageDistanceCampsiteLocationsTrack} computes the mean distance between
#' locations of a \code{\link[trajectories:Track]{Track}} in the order of movement.
#' Several options are available.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track]{Track}}
#' with a variable \code{location} and \code{campsite}
#' @param fun One of \code{mean} or \code{median)}, depending on which function should be
#' used in order to compute average values.
#' @return a numeric value representing the number of unique (campsite) locations
#' in \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
averageDistanceCampsiteLocationsTrack <- function(currenttrack, fun = median){

  # checks
  if(!(inherits(currenttrack, "Track"))) {
    stop("currenttrack must be a Track object\n")
  }
  if(!(any(colnames(currenttrack@data) == "location") && any(colnames(currenttrack@data) == "campsite"))) {
    stop("the currenttrack@data must contain a variable 'location' and 'campsite'\n")
  }
  if(!(is.function(fun))) {
    stop("fun must be a function")
  }

  # get an index of entries refering to campsite locations that are not duplicated and no gaps
  sel <- currenttrack@data
  index <- which(sel$location != 0 & sel$campsite & !duplicated(sel$location))

  # extract the corresponding values and create a new Track
  newcurrenttrack <- trajectories::Track(track = spacetime::STIDF(sp = currenttrack@sp[index,],
                                               time = currenttrack@time[index],
                                               data = sel[index,],
                                               endTime = currenttrack@time[index]))

  # compute the average distances
  fun(newcurrenttrack@connections$distance)

}
