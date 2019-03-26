#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Computes the sum of the longitudinal transitions of a Track object.
#'
#' \code{sumLongitudeTrack} sums the longitude differences between adjacent campsite
#' locations for a \code{\link[trajectories:Track]{Track}}.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track]{Track}}
#' with a variable \code{location} and \code{campsite} in the data slot.
#' @return a numeric value representing the sum of the longitude transitions
#' for \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
sumLongitudeTrack <- function(currenttrack){

  # checks
  if(!(inherits(currenttrack, "Track"))) {
    stop("currenttrack must be a Track object\n")
  }
  if(!(any(colnames(currenttrack@data) %in% c("location", "campsite")))) {
    stop("currenttrack must have the variables 'location' and 'campsite'\n")
  }

  # get an index of entries refering to campsite locations that are not duplicated and no gaps
  sel <- currenttrack@data
  index <- which(sel$location != 0 & sel$campsite & !duplicated(sel$location))

  # extract the corresponding values and create a new Track
  newcurrenttrack <- trajectories::Track(track = spacetime::STIDF(sp = currenttrack@sp[index,],
                                                                  time = currenttrack@time[index],
                                                                  data = sel[index,],
                                                                  endTime = currenttrack@time[index]))

  # compute the sum of the longitude transitions
  longitude <- newcurrenttrack@sp@coords[,1]
  sum(longitude[-1]-longitude[-length(longitude)])

}
