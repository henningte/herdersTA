#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import raster
NULL

#' Extracts corresponding raster values for GPS tracks.
#'
#' \code{extractRasterTrackSingleLayer} extracts values from \code{RasterLayer}
#' (see: \code{\link[raster]{Raster-class}}) that corresponds to the the respective
#' position of a \code{\link[trajectories]{Track}} object with
#' daily resolution.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object that
#' has a column \code{day} containing the time information of the data
#' values.
#' @param raster A \code{RasterLayer} (see: \code{\link[raster]{Raster-class}}).
#' from which values should be extracted.
#' @param buffer A numeric value indicating the radius of the buffer
#' around each point that should be considered during extraction of
#' the raster values. If the data are not projected
#' (latitude/longitude), the unit should be meters. Otherwise it should
#' be in map-units (typically also meters).
#' @return A vector with a value for each data value of \code{currenttrack}.
#' If a value of \code{raster} cannot be assigned to a respective data value,
#' \code{NA} is returned for the respective data value.
#' @seealso \code{\link{assignFixedTenDayInterval}},
#' \code{\link{extractRasterTracks}}.
#' @examples #
#' @export
extractRasterTrackSingleLayer <- function(currenttrack,
                                          raster,
                                          buffer = 0
){

  # check if currenttrack is specified correctly
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack must be an object of class Track\n")
  }
  if(!all(c("day", "longitude", "latitude") %in% colnames(currenttrack@data))){
    stop("currenttrack must have columns 'day', 'longitude' and 'latitude'\n")
  }

  # extract the position of each location from currenttrack
  locationsdata <- currenttrack@data[!duplicated(currenttrack@data$location), c(2,3,4)]
  locationsdata <- SpatialPointsDataFrame(coords = locationsdata[,c(2,3)], data = locationsdata, proj4string = CRS(proj4string(currenttrack@sp)))

  # extract the location for each day
  eachdaylocation <- currenttrack@data$location

  # define an index for nongaps
  indexnongaps <- which(eachdaylocation != 0)

  # create a vector to store the results in
  res <- rep(NA, length(eachdaylocation))

  # extract all values from locationsdata and raster that are no gaps
  eachdaylocation <- eachdaylocation[indexnongaps]

  # extract the values for all locations
  extractedvalues <- do.call(cbind, raster::extract(x = raster, y = locationsdata, method = "simple", buffer = buffer, na.rm = TRUE))

  # filter the values for each day for the target location
  extractedvalues <- sapply(seq_along(eachdaylocation), function(x) extractedvalues[which(locationsdata$location == eachdaylocation[x])])

  # insert the values into res
  res[indexnongaps] <- extractedvalues

  # return res
  return(res)

}
