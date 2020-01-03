#' @importFrom trajectories Track
#' @importFrom spacetime STIDF
NULL

#' Computes the straightness index for the trajectory between the first and last campsite location in a Track object.
#'
#' \code{straighntnessindexCampsiteLocationsTrack} computes the straightness index
#' for the trajectory between the first and last locations of a
#' \code{\link[trajectories:Track-class]{Track}} in the order of movement.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location} and \code{campsite}
#' @return a numeric value representing the straightness index for
#' \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
straighntnessindexCampsiteLocationsTrack <- function(currenttrack){

  # checks
  if(!(inherits(currenttrack, "Track"))) {
    stop("currenttrack must be a Track object\n")
  }
  if(!(any(colnames(currenttrack@data) == "location") && any(colnames(currenttrack@data) == "campsite"))) {
    stop("the currenttrack@data must contain a variable 'location' and 'campsite'\n")
  }

  # get an index of entries refering to campsite locations that are not duplicated and no gaps
  sel <- currenttrack@data
  index <- which(sel$location != 0 & sel$campsite & !duplicated(sel$location))

  # extract the corresponding values and create a new Track
  newcurrenttrack1 <- trajectories::Track(track = spacetime::STIDF(sp = currenttrack@sp[index,],
                                                                  time = currenttrack@time[index],
                                                                  data = sel[index,],
                                                                  endTime = currenttrack@time[index]))
  newcurrenttrack2 <- trajectories::Track(track = spacetime::STIDF(sp = currenttrack@sp[index[c(1, length(index))],],
                                                                   time = currenttrack@time[index[c(1, length(index))]],
                                                                   data = sel[index[c(1, length(index))],],
                                                                   endTime = currenttrack@time[index[c(1, length(index))]]))

  # compute the average distances
  sum(newcurrenttrack1@connections$distance)/sum(newcurrenttrack2@connections$distance)

}
