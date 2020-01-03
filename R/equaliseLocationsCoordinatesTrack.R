#' @importFrom sp coordinates SpatialPoints proj4string CRS
#' @importFrom spacetime STIDF
#' @importFrom trajectories Track
#' @importFrom tidyr fill
#' @importFrom dplyr left_join
#' @importFrom stats median
NULL

#' Equalise Track Coordinates to Locations
#'
#' \code{equaliseLocationsCoordinatesTrack} computes the median
#' coordinates (longitude, latitude) for each location of a track
#' (object of class \code{\link[trajectories:Track-class]{Track}})
#' and assigns these coordinates to matching data values in the track.
#' During this procedure, filled values are not considered in order to
#' not influence the median coordinates by the gap filling procedure.
#' However, filled values are considered if there are only filled values
#' for a location. This may be the case if non-gap values are only
#' present during day and at the borders of a visit.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object
#' with a variable \code{location} and a variable \code{filled} in the data slot.
#' \code{location} must be numeric with an integer value for each unique location.
#' Gaps must have the location \code{0}.
#' @return \code{currenttrack} with the same longitude and latitude values for
#' each location and the corresponding data values.
#' @seealso \code{\link{locationsTrack}}.
#' @examples #
#' @export
equaliseLocationsCoordinatesTrack <- function(currenttrack){

  # checks
  if(!(inherits(currenttrack, "Track") || "location" %in% colnames(currenttrack@data))){
    stop("currenttrack must be a Track object with a variable 'location'\n")
  }
  if(!(all(currenttrack$location %% 1 == 0) || is.numeric(location))){
    stop("currenttrack$location must be numeric with integer values")
  }

  # extract the locations
  xlocations <- data.frame(location = currenttrack$location,
                           stringsAsFactors = FALSE)

  # extract the coordinates
  xcoords <- sp::coordinates(currenttrack@sp)

  # compute median coordinates for each location
  xcoordsperlocation <- data.table::rbindlist(tapply(seq_len(nrow(xcoords)), currenttrack$location, function(x){
    if(all(currenttrack$filled[x])){
      notfilled <- currenttrack$filled[x]
    }else{
      notfilled <- !currenttrack$filled[x]
    }
    mediancoords <- apply(xcoords[x[notfilled], , drop = FALSE], 2, function(y) stats::median(y, na.rm = TRUE))
    data.frame(location = currenttrack$location[x[1]],
               longitude = mediancoords[1],
               latitude = mediancoords[2],
               stringsAsFactors = FALSE)
  }, simplify = FALSE))

  # set xcoordsperlocation to NA for location == 0
  xcoordsperlocation[xcoordsperlocation$location == 0, 2:3] <- NA

  # merge the values
  xlocations <- dplyr::left_join(x = xlocations,
                                 y = xcoordsperlocation,
                                 by = "location")

  # fill in coordinates for gaps
  xlocations <- tidyr::fill(xlocations,
                            seq_len(ncol(xlocations)),
                            .direction = "up")
  xlocations <- tidyr::fill(xlocations,
                            seq_len(ncol(xlocations)),
                            .direction = "down")

  # add the new coordinates to currenttrack
  trajectories::Track(spacetime::STIDF(sp = sp::SpatialPoints(coords = xlocations[,2:3],
                                                              proj4string = sp::CRS(sp::proj4string(currenttrack))),
                                       time = currenttrack@time,
                                       data = currenttrack@data,
                                       endTime = currenttrack@time))

}
