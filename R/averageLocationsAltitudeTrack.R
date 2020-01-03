#' @importFrom stats median
NULL

#' Computes the average altitude of locations of a Track object.
#'
#' \code{averageLocationsAltitudeTrack} computes the average altitude of lcoations of a
#' \code{\link[trajectories:Track-class]{Track}}. Several options are available. For each
#' location, only one value is considered (i.e. thetemporal duration of visits is not
#' considered as weights during computations).
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{altitude} and a variable \code{location} and \code{campsite}
#' as created by \code{\link{locationsTrack}} in the data slot.
#' @param fun One of \code{mean} or \code{median)}, depending on which function should be
#' used in order to compute average values.
#' @param campsite A logical value indicating if only campsites should be considered
#' (\code{TRUE}) or any locations (\code{FALSE}).
#' @return a numeric value representing the average altitude of the locations in \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
averageLocationsAltitudeTrack <- function(currenttrack, fun = stats::median, campsite = TRUE){

  # checks
  if(!(inherits(currenttrack, "Track"))) {
    stop("currenttrack must be a Track object\n")
  }
  if(!(any(colnames(currenttrack@data) == "altitude") && any(colnames(currenttrack@data) == "location") && any(colnames(currenttrack@data) == "campsite"))) {
    stop("the currenttrack@data must contain a variable 'altitude', 'location' and 'campsite'\n")
  }
  if(!(is.function(fun))) {
    stop("fun must be a function")
  }
  if(!(is.logical(campsite) && length(campsite) == 1)) {
    stop("campsite must be a logical value\n")
  }

  # remove duplicated values and gaps
  sel <- currenttrack@data
  sel <- sel[!duplicated(sel$location) & sel$location != 0,]

  # remove locations related to short-term visits
  if(campsite) {
    sel <- sel[sel$campsite,]
  }

  # compute the average value
  fun(sel$altitude)

}
