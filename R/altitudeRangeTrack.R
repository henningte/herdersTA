#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Extracts the altitude range from a Track object.
#'
#' \code{altitudeRangeTrack} extracts the altitude range
#' \code{\link[trajectories:Track]{Track}} object with a variable
#' \code{altitude} in the data slot. \code{NA}s are removed.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track]{Track}}
#' with a variable \code{altitude} in the data slot.
#' @return a numeric vector of length two with the range of the altitude values
#' of \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
altitudeRangeTrack <- function(currenttrack){

  # checks
  if(!(inherits(currenttrack, "Track"))) {
    stop("currenttrack must be a Track object\n")
  }
  if(!(any(colnames(currenttrack@data) == "altitude"))) {
    stop("the currenttrack@data must contain a variable 'altitude'\n")
  }

  # compute the altitude range
  range(currenttrack$altitude, na.rm = TRUE)

}
