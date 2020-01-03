#' @importFrom sp spDists SpatialLines Lines
#' @importFrom rgeos gDistance
NULL
#' Estimates the linearity of a Track object.
#'
#' \code{linearityCampsiteLocationsTrack} computes an estimate of the
#' linearity of a trajectory (object of class \code{\link[trajectories:Track-class]{Track}})
#' by (1) extracting the maximum distance between any two locations,
#' (2) extracting the maximum distance between any two remaining locations
#' orthogonal to the first line and (3) dividing the values extracted in (1) and (2).
#' The larger the value is the more linear is the trajectory overall.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location} and \code{campsite}
#' @return a numeric value representing the straightness index for
#' \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
linearityCampsiteLocationsTrack <- function(currenttrack){

  # checks
  if(!(inherits(currenttrack, "Track"))) {
    stop("currenttrack must be a Track object\n")
  }
  if(!(any(colnames(currenttrack@data) == "location") && any(colnames(currenttrack@data) == "campsite"))) {
    stop("the currenttrack@data must contain a variable 'location' and 'campsite'\n")
  }

  # get an index of entries refering to campsite locations that are not duplicated and no gaps
  sel <- currenttrack@data
  index <- which(sel$location != 0 & sel$campsite & !duplicated(sel$location))

  # extract the locations
  sel_locations <- currenttrack@sp[index,]

  # compute distance between points
  sel_locations_distances <- sp::spDists(sel_locations, longlat = FALSE)

  # get maximum distance
  sel_locations_distance_max1 <- which.max(apply(sel_locations_distances, 2, max))
  sel_locations_distance_max2 <- which.max(sel_locations_distances[,sel_locations_distance_max1])
  sel_locations_distance_max <- max(sel_locations_distances)

  # construct a line between point 1 and 2
  sel_locations_distance_line1 <- sp::SpatialLines(list(sp::Lines(sp::Line(sel_locations[c(sel_locations_distance_max1, sel_locations_distance_max2),]), ID = "a")))

  # compute distance between points and line
  sel_locations_distance_to_line1 <- rgeos::gDistance(sel_locations, sel_locations_distance_line1, byid = TRUE)

  # get all possible pairs of points
  points_combinations <- utils::combn(seq_along(sel_locations_distance_to_line1), 2)

  # for each pair of points: sum the distances
  sel_locations_distance_to_line_max <- max(apply(points_combinations, 2, function(x){
    sum(sel_locations_distance_to_line1[x])
  }))

  # compute the ratio
  sel_locations_distance_max/sel_locations_distance_to_line_max
}
