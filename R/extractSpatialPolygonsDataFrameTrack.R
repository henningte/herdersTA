#' @importFrom sp SpatialPointsDataFrame CRS proj4string
NULL

#' Assigns \code{SpatialPolygonsDataFrame} values to values of a GPS tracks.
#'
#' \code{extractSpatialPolygonsDataFrameTrack} extracts for each data
#' value of a \code{\link[trajectories:Track-class]{Track}} object a value
#' of a \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} object of a specified
#' variable.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object that
#' has a column \code{time} containing the time information of the data
#' values.
#' @param sppdf A \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} object.
#' @param variable A \code{character} value indicating for which variable
#' to extract values from \code{sppdf}.
#' @return A vector with a value for each data value of \code{currenttrack}
#' extracted from the speified variable of \code{sppdf}.
#' If a value of \code{raster} cannot be assigned to a respective data value,
#' \code{NA} is returned for the respective data value.
#' @seealso .
#' @examples #
#' @export
extractSpatialPolygonsDataFrameTrack <- function(currenttrack,
                                                 sppdf,
                                                 variable) {

  # check if currenttrack is of class Track
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack has to be of class Track\n")
  }

  # check if sppdf is a SpatialPolygonsDataFrame
  if(!inherits(sppdf, "SpatialPolygonsDataFrame")){
    stop("sppdf has to be of class SpatialPolygonsDataFrame\n")
  }

  # check if variable is specified correctly
  if(!(is.character(variable) & length(variable) == 1)){
    stop("variable must be a character value\n")
  }

  # extract the position of each location from currenttrack
  locationsdata <- currenttrack@data[!duplicated(currenttrack@data$location), c(2,3,4)]
  locationsdata <- sp::SpatialPointsDataFrame(coords = locationsdata[,c(2,3)],
                                              data = locationsdata,
                                              proj4string = sp::CRS(sp::proj4string(currenttrack@sp)))

  # extract the location for each day
  eachdaylocation <- currenttrack@data$location

  # define an index for nongaps
  indexnongaps <- which(eachdaylocation != 0)

  # create a vector to store the results in
  res <- rep(NA, length(eachdaylocation))

  # extract all values from locationsdata and raster that are no gaps
  eachdaylocation <- eachdaylocation[indexnongaps]

  # get the id of the variable to extract
  variableid <- match(x = variable, table = colnames(sppdf@data))

  # extract the values for all locations
  extractedvalues <- unlist(over(x = locationsdata, y = sppdf[,variableid], returnList = TRUE))

  # filter the values for each day for the target location
  extractedvalues <- sapply(seq_along(eachdaylocation), function(x) extractedvalues[which(locationsdata$location == eachdaylocation[x])])

  # insert the values into res
  res[indexnongaps] <- extractedvalues

  # return res
  res

}
