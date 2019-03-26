#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Computes the total distance covered by a Track object.
#'
#' \code{totalDistanceTrack} computes the total distance covered by a
#' \code{\link[trajectories:Track]{Track}}. This is the sum of all distance
#' values in the \code{connections} slot.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track]{Track}}.
#' @return a numeric value representing the total distance covered by \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
totalDistanceTrack <- function(currenttrack){

  # compute the total horizontal distance
  sum(currenttrack@connections$distance)

}
