#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import sp
NULL

#' Assigns \code{\link[sp]{SpatialPolygonsDataFrame}} values to values of a GPS tracks.
#'
#' \code{extractSpatialPolygonsDataFrameTrack} extracts for each data
#' value of a \code{\link[trajectories]{Track}} object a value
#' of a \code{\link[sp]{SpatialPolygonsDataFrame}} object of a specified
#' variable.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object that
#' has a column \code{time} containing the time information of the data
#' values.
#' @param sppdf A \code{\link[sp]{SpatialPolygonsDataFrame}} object.
#' @param variable A \code{character} value indicating for which variable
#' to extract values from \code{sppdf}.
#' @return A vector with a value for each data value of \code{currenttrack}
#' extracted from the speified variable of \code{sppdf}.
#' If a value of \code{raster} cannot be assigned to a respective data value,
#' \code{NA} is returned for the respective data value.
#' @seealso .
#' @examples #
#' @export
extractSpatialPolygonsDataFrameTrack <- function(currenttrack, sppdf, variable){



}
