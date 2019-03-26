#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Computes the total altitudinal distance covered by a Track object.
#'
#' \code{totalAltitudeDistanceTrack} computes the total altitudinal distance
#' covered by a \code{\link[trajectories:Track]{Track}} (sum of absolute altitudinal
#' distances).
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track]{Track}}
#' with a variable \code{altitude} in the \code{data} slot.
#' @return a numeric value representing the total altitudinal distance covered by
#' \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
totalAltitudeDistanceTrack <- function(currenttrack){

  # compute the latitude difference
  sum(abs(currenttrack@data$altitude[-1] - currenttrack@data$altitude[-length(currenttrack)]))

}
