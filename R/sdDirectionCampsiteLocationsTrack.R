#' @importFrom stats median sd
#' @importFrom trajectories Track
#' @importFrom spacetime STIDF
NULL

#' Computes the standard eviation of the direction (as absolute difference of angles) between campsite locations of a Track object in the order of movement.
#'
#' \code{sdDirectionCampsiteLocationsTrack} computes the standard deviation of directions
#' between locations of a \code{\link[trajectories:Track-class]{Track}} in the order of movement.
#' Directions are computed as angle difference between the previous campsite location
#' and the current campsite location and the next campsite location and the current
#' campsite location. Several options are available.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location} and \code{campsite}.
#' @return a numeric value representing the standard deviation of directions between
#' campsites in the order of movement.
#' @seealso .
#' @examples #
#' @export
sdDirectionCampsiteLocationsTrack <- function(currenttrack, fun = stats::median){

  # checks
  stopifnot(inherits(currenttrack, "Track"))

  # get an index of entries refering to campsite locations that are not duplicated and no gaps
  sel <- currenttrack@data
  index <- which(sel$location != 0 & sel$campsite & !duplicated(sel$location))

  # extract the corresponding values and create a new Track
  newcurrenttrack <- trajectories::Track(track = spacetime::STIDF(sp = currenttrack@sp[index,],
                                                                  time = currenttrack@time[index],
                                                                  data = sel[index,],
                                                                  endTime = currenttrack@time[index]))

  # function in order to compute the difference between two angels
  angdiff <- function(x, y){
    phi <- abs(x - y) %% 360
    ifelse(phi > 180, 360 - phi, phi)
  }

  # compute the standard deviation of directions
  stats::sd(angdiff(newcurrenttrack@connections$direction[-1], newcurrenttrack@connections$direction[-nrow(newcurrenttrack@connections)]))

}
