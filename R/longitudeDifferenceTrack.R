#' Computes the latitude difference between the most western and most eastern points of a Track object.
#'
#' \code{longitudeDifferenceTrack} computes the longitudinal difference between
#' the most western and most eastern points of a
#' \code{\link[trajectories:Track-class]{Track}}.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{altitude} in the data slot.
#' @return a numeric value representing the longitudinal difference between the
#' most western and most eastern points in \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
longitudeDifferenceTrack <- function(currenttrack){

  # compute the longitude difference
  diff(range(currenttrack@sp@coords[,1]))

}
