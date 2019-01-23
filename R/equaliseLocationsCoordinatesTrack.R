#' @importFrom Rdpack reprompt
#' @import trajectories Track
#' @importFrom sp coordinates
NULL

#' Equalise Track Coordinates to Locations
#'
#' \code{equaliseLocationsCoordinatesTrack} computes the median
#' coordinates (longitude, latitude) for each location of a track
#' (object of class \code{\link[trajectories:Track-class]{Track}})
#' and assigns these coordinates to matching data values in the track.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object
#' with a variable \code{location} in the data slot. \code{location} must be
#' numeric with an integer value for each unique location. Gaps must have the
#' location \code{0}.
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
  xlocations <- data.frame(location = currenttrack$location)

  # extract the coordinates
  xcoords <- sp::coordinates(currenttrack@sp)

  # compute median coordinates for each location
  xcoordsperlocation <- tapply(seq_len(nrow(xcoords)), currenttrack$location, function(x){
    apply(xcoords[x,], 2, function(y) median(y, na.rm = TRUE))
  })

  # merge the values
  xlocations <- merge(y = xlocations, x = xcoords, by = "location", all.x = TRUE, sort = FALSE)

  # add the new coordinates to currenttrack
  sp::coordinates(currenttrack@sp) <- xlocations[,2:3]

  # return currenttrack
  return(currenttrack)

}
