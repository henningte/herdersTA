#' Computes the number of unique locations of a Track object.
#'
#' \code{countUniqueLocationsTrack} counts unique lcoations of a
#' \code{\link[trajectories:Track-class]{Track}}. Several options are available.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location}, \code{campsite} and \code{norepeatedcampsitevisits}.
#' as created by \code{\link{locationsTrack}} in the data slot.
#' @param campsite A logical value indicating if only campsites should be considered
#' (\code{TRUE}) or any locations (\code{FALSE}).
#' @param repeated A logical value indicating if the number of locations with repeated
#' visits shoud be counted (\code{TRUE}) or not (\code{FALSE}). This is only available
#' with \code{campsite = TRUE}.
#' @return a numeric value representing the number of unique (campsite) locations
#' in \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
countUniqueLocationsTrack <- function(currenttrack, campsite = TRUE, repeated = FALSE){

  # checks
  if(!(inherits(currenttrack, "Track"))) {
    stop("currenttrack must be a Track object\n")
  }
  if(!(any(colnames(currenttrack@data) == "location") && any(colnames(currenttrack@data) == "campsite") && any(colnames(currenttrack@data) == "norepeatedcampsitevisits"))) {
    stop("the currenttrack@data must contain a variable 'altitude', 'location' and 'campsite'\n")
  }
  if(!(is.logical(campsite) && length(campsite) == 1)) {
    stop("campsite must be a logical value\n")
  }
  if(!(is.logical(repeated) && length(repeated) == 1)) {
    stop("repeated must be a logical value\n")
  }
  if(!campsite && repeated) {
    stop("repeated = TRUE is only supported with campsite = TRUE\n")
  }

  # extract the data
  sel <- currenttrack@data

  if(!repeated) {

    # remove locations related to short-term visits
    if(campsite) {
      sel <- sel[sel$campsite,]
    }

    # remove duplicated values and gaps
    sel <- sel[!duplicated(sel$location) & sel$location != 0,]

    # count the locations
    nrow(sel)

  } else {

    # remove gaps and entries for short-term visits
    sel <- sel[sel$campsite & sel$location != 0,]

    # get the maximum number of sure repeated campsite visits per location
    sel <- tapply(sel$norepeatedcampsitevisits, sel$location, function(x) {
      max(na.omit(x))
    })

    # count the number of entries >= 1
    sum(sapply(sel, function(x) x > 1))

  }



}
