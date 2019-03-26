#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Computes the proportion of gaps in a Track object.
#'
#' \code{gapProportionTrack} computes the proportion of gaps in a
#' \code{\link[trajectories:Track]{Track}}.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track]{Track}}
#' with a variable \code{location}.
#' @return A numeric value representing the proportion of gaps
#' in \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
gapProportionTrack <- function(currenttrack){

  # checks
  if(!(inherits(currenttrack, "Track"))) {
    stop("currenttrack must be a Track object\n")
  }
  if(!(any(colnames(currenttrack@data) == "location"))) {
    stop("fun must be a function")
  }

  # compute the proportion of gaps
  length(which(currenttrack@data$location == 0))/nrow(currenttrack@data)

}
