#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Computes the average direction (as absolute difference of angles) between campsite locations of a Track object in the order of movement.
#'
#' \code{averageDirectionCampsiteLocationsTrack} computes the average direction between
#' locations of a \code{\link[trajectories:Track]{Track}} in the order of movement.
#' Directions are computed as angle difference between the previous campsite location
#' and the current campsite location and the next campsite location and the current
#' campsite location. Several options are available.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track]{Track}}
#' with a variable \code{location} and \code{campsite}
#' @param fun One of \code{mean} or \code{median)}, depending on which function should be
#' used in order to compute average values.
#' @return a numeric value representing the average direction between
#' locations in \code{currenttrack} in the order of movement.
#' @seealso .
#' @examples #
#' @export
averageDirectionCampsiteLocationsTrack <- function(currenttrack, fun = median){

  # checks
  stopifnot(inherits(currenttrack, "Track"))
  stopifnot(is.function(fun))

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

  # compute the average directions
  fun(angdiff(newcurrenttrack@connections$direction[-1], newcurrenttrack@connections$direction[-nrow(newcurrenttrack@connections)]))

}
