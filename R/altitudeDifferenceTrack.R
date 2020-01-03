#' Computes the altitude difference between the highest and lowest point of a Track object.
#'
#' \code{altitudeDifferenceTrack} computes the altitudinal difference between
#' the highest and the lowest point of a
#' \code{\link[trajectories:Track-class]{Track}} object with a variable
#' \code{altitude} in the data slot. \code{NA}s are removed.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{altitude} in the data slot.
#' @return a numeric value representing the altitudinal difference between the
#' highest and the lowest point in \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
altitudeDifferenceTrack <- function(currenttrack){

  # compute the altitude difference
  diff(altitudeRangeTrack(currenttrack))

}
