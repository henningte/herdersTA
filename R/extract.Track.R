#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import raster
NULL

#' Extracts corresponding raster values for GPS tracks.
#'
#' \code{extract.Track} is the \code{\link[raster:extract]{extract}} method
#' in order to extract values from \code{\link[raster:raster]{Raster*}} objects
#' for an object of class \code{\link[trajectories:Track-class]{Track}}. Different
#' options are available.
#'
#' @param x A Raster* object. \code{x} must either have one layer or
#' the number of layers of \code{x} has to be the same as the number of
#' data values in \code{y}. If a multi-layer object is provided, it
#' is automatically assumed that this is the case.
#' @param y A \code{\link[trajectories:Track-class]{Track}} object.
#' Depending on other parameters set, there may be certain variables required.
#' @param buffer A numeric value indicating the radius of the buffer
#' around each point that should be considered during extraction of
#' the raster values. If the data are not projected
#' (latitude/longitude), the unit should be meters. Otherwise it should
#' be in map-units (typically also meters).
#' @param small logical. If \code{TRUE} and a \code{buffer} argument is used,
#' the function always returns a number, also when the buffer does not include
#' the center of a single cell. The value of the cell in which the point falls
#' is returned if no cell center is within the buffer.
#' @param fun function to summarize the values (e.g. \code{mean}). The function
#' should take a single numeric vector as argument and return a single value
#' and accept a \code{na.rm} argument.
#' @param na.rm logical. Only useful when an argument fun is supplied. If
#' \code{na.rm = TRUE} (the default value), \code{NA} values are removed
#' before \code{fun} is applied. This argument may be ignored if the function
#' used has a \code{...} argument and ignores an additional \code{na.rm} argument.
#' @param location A logical value indicating if for each location in \code{y}
#' the same position is assumed for all data values. In this case, computation
#' can be speed up by setting \code{location = TRUE}.
#' @return A vector with a value for each data value of \code{currenttrack}.
#' If a value of \code{raster} cannot be assigned to a respective data value,
#' \code{NA} is returned for the respective data value.
#' @seealso \code{\link{assignFixedTenDayInterval}},
#' \code{\link{extractRasterTracks}}.
#' @examples #
#' @export
extractTrack <- function(x, y, buffer = 0, small = TRUE, fun = NULL, na.rm = TRUE, location = FALSE
){

  # checks
  if(!(inherits(y, "Track"))){
    stop("y must be a Track object\n")
  }
  if(!(is.logical(location) || length(location) != 1)){
    stop("location must be a logical value\n")
  }
  if(!(location && "location" %in% colnames(y@data))){
    stop("y does not contain a variable location\n")
  }

  # number of layers in x
  xnlayers <- nlayers(x)

  # check if xnlayers == 1 || nlayers == nrow(y@data)
  if(!(length(xnlayers == 1 || nlayers == nrow(y@data)))){
    stop("x must contain either one layer or the number of layers of x must match the number of values in y\n")
  }

  # define an index for non gaps
  nongapsindex <- which(y@data$location != 0)

  # define the points to extract from
  if(!location){
    ypts <- y@sp[nongapsindex]
  }else{
    uniquelocationindex <- which(!duplicated(y@data$location))
    ypts <- sp::SpatialPointsDataFrame(coords = y@sp[uniquelocationindex], data = data.frame(location = y@data$location[uniquelocationindex]), proj4string = CRS(proj4string(y)))
  }

  # define parameter layer
  layer <- 1

  if(xnlayers == 1){

    # extract the values for all locations
    res <- raster::extract(x = x, y = ypts, method = "simple", buffer = buffer, na.rm = na.rm, layer = layer, nl = xnlayers, fun = fun, df = TRUE)[,2]

  }else{

    # extract the values for all locations
    res <- raster::extract(x = x, y = ypts, method = "simple", buffer = buffer, na.rm = na.rm, layer = layer, nl = xnlayers, fun = fun)

    # select only those values ... todo

  }

  if(location){

    # add res to ypts
    ypts$res <- res

    # merge the values to currenttrack@data and keep only res
    dplyr::left_join(x = y@data, y = ypts@data, by = "location")$res

  }else{

  }

}

