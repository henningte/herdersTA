#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Computes the latitude difference between the most northern and most southern points of a Track object.
#'
#' \code{latitudeDifferenceTrack} computes the latitudinal difference between
#' the most northern and most southern points of a
#' \code{\link[trajectories:Track]{Track}}.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track]{Track}}
#' with a variable \code{altitude} in the data slot.
#' @return a numeric value representing the latitudinal difference between the
#' most northern and most southern points in \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
latitudeDifferenceTrack <- function(currenttrack){

  # compute the latitude difference
  diff(range(currenttrack@sp@coords[,2]))

}
