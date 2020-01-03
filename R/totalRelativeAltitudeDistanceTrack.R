#' Computes the total altitudinal distance covered by a Track object.
#'
#' \code{totalRelativeAltitudeDistanceTrack} computes the total altitudinal distance
#' covered by a \code{\link[trajectories:Track-class]{Track}} (sum of absolute altitudinal
#' distances).
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{altitude} in the \code{data} slot.
#' @return a numeric value representing the total altitudinal distance covered by
#' \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
totalRelativeAltiduinalDistanceTrack <- function(currenttrack){

  # compute the altitude distance
  totalAltitdeDistanceTrack(currenttrack)/totalDistanceTrack(currenttrack)

}
