#' @importFrom raster area
NULL

#' Computes various summary indicators for a Track object.
#'
#' \code{siTrack} is a wrapper function for functions that takes a \code{Track}
#' object as input and return numeric values/vectors as output that summarise the
#' \code{Track} object. The functions are:
#' \enumerate{
#'   \item \code{\link{averageLocationsAltitudeTrack}}
#'   \item \code{\link{altitudeRangeTrack}}
#'   \item \code{\link{altitudeDifferenceTrack}}
#'   \item \code{\link{averageDistanceCampsiteLocationsTrack}}
#'   \item \code{\link{longitudeDifferenceTrack}}
#'   \item \code{\link{latitudeDifferenceTrack}}
#'   \item \code{\link{sumLongitudeTrack}}
#'   \item \code{\link{sumLatitudeTrack}}
#'   \item \code{\link{chullTrack}}: the area and circumference are extracted
#'   from the \code{SpatialPolygons} object.
#'   \item \code{\link{averageDirectionCampsiteLocationsTrack}}
#'   \item \code{\link{countUniqueLocationsTrack}} (number of unique campsite locations
#'   and number of campsite locations with repeated campsite visits).
#'   \item \code{\link{gapProportionTrack}}
#' }
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location} and \code{campsite}
#' @param fun One of \code{mean} or \code{median)}, depending on which function should be
#' used in order to compute average values.
#' @param campsite A logical value indicating if only campsites should be considered
#' (\code{TRUE}) or any locations (\code{FALSE}).
#' @return a numeric value representing the number of unique (campsite) locations
#' in \code{currenttrack}.
#' @seealso .
#' @examples #
#' @export
siTrack <- function(currenttrack, fun = stats::median){

  # checks
  if(!(inherits(currenttrack, "Track"))) {
    stop("currenttrack must be a Track object\n")
  }
  if(!(is.function(fun))) {
    stop("fun must be a function")
  }
  if(!(any(colnames(currenttrack@data) == "location") && any(colnames(currenttrack@data) == "campsite") && any(colnames(currenttrack@data) == "norepeatedcampsitevisits"))) {
    stop("the currenttrack@data must contain a variable 'altitude', 'location' and 'campsite'\n")
  }

  # compute the summary indicators
  campsite <- TRUE
  location_altitude <- averageLocationsAltitudeTrack(currenttrack, fun = fun)
  location_altitude_range <- altitudeRangeTrack(currenttrack = currenttrack)
  location_altitude_difference <- altitudeDifferenceTrack(currenttrack)
  total_altitude_distance <- totalAltitudeDistanceTrack(currenttrack)
  total_relative_altitude_distance <- totalRelativeAltiduinalDistanceTrack(total_relative_altitude_distance)
  location_distance <- averageDistanceCampsiteLocationsTrack(currenttrack, fun = fun)
  total_distance = totalDistanceTrack(currenttrack)
  longitude_difference <- longitudeDifferenceTrack(currenttrack)
  latitude_difference <- latitudeDifferenceTrack(currenttrack)
  longitude_sum <- sumLongitudeTrack(currenttrack)
  latitude_sum <- sumLatitudeTrack(currenttrack)
  chull_area <- raster::area(chullTrack(currenttrack))
  chull_perimeter <- SpatialLinesLengths(as(chullTrack(currenttrack), "SpatialLines"), longlat = FALSE) * 1000
  locations_direction <- averageDirectionCampsiteLocationsTrack(currenttrack, fun = fun)
  locations_direction_sd <- sdDirectionCampsiteLocationsTrack(currenttrack)
  locations_number <- countUniqueLocationsTrack(currenttrack, campsite = campsite, repeated = FALSE)
  locations_number_withrepeated <- countUniqueLocationsTrack(currenttrack, campsite = campsite, repeated = TRUE)
  gaps_proportion <- gapProportionTrack(currenttrack)
  linearity <- linearityCampsiteLocationsTrack(currenttrack)

  # collect all variables in a data.frame
  data.frame(
    location_altitude = location_altitude,
    location_altitude_min = min(location_altitude_range),
    location_altitude_max = max(location_altitude_range),
    location_altitude_difference = location_altitude_difference,
    total_altitude_distance = total_altitude_distance,
    total_relative_altitude_distance = total_relative_altitude_distance,
    location_distance = location_distance,
    total_distance =total_distance,
    longitude_difference = longitude_difference,
    latitude_difference = latitude_difference,
    longitude_sum = longitude_sum,
    latitude_sum = latitude_sum,
    chull_area = chull_area,
    chull_perimeter = chull_perimeter,
    locations_direction = locations_direction,
    locations_direction_sd = locations_direction_sd,
    locations_number = locations_number,
    locations_number_withrepeated = locations_number_withrepeated,
    gaps_proportion = gaps_proportion,
    linearity = linearity,
    stringsAsFactors = FALSE
  )

}
