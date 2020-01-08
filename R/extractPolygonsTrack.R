#' @importFrom sp coordinates SpatialPolygonsDataFrame proj4string CRS over
#' @importFrom dplyr left_join
NULL

#' Extracts corresponding raster values for GPS tracks.
#'
#' \code{extractPolygonsTrack} is a function
#' in order to extract values from \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} objects
#' for an object of class \code{\link[trajectories:Track-class]{Track}}.
#'
#' @param x A \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} object.
#' @param y A \code{\link[trajectories:Track-class]{Track}} object.
#' Depending on other parameters set, there may be certain variables required.
#' @param fn function to summarize the values (e.g. \code{mean}).
#' @param ... further arguments passed to \code{\link[sp]{over}}.
#' @param fixedlocationcoords A logical value indicating if for each location in \code{y}
#' the same position is assumed for all data values. In this case, computation
#' can be speed up by setting \code{location = TRUE} and the function uses
#' simply the coordinates of the first value for each location.
#' @param what A numeric vector specifying the column(s) of \code{x}
#' from which to retain values. I fset to \code{NULL}, values for all
#' columns will be extracted.
#' @return A data.frame with a value for each data value of \code{y} in
#' columns specified with \code{what}.
#' If a value of \code{x} cannot be assigned to a respective data value,
#' \code{NA} is returned for the respective data value.
#' @seealso
#' \code{\link{extractRasterTrack}},
#' \code{\link{extractPolygonsTracks}}.
#' @examples #
#' @export
extractPolygonsTrack <- function(x,
                                 y,
                                 fn = NULL,
                                 ...,
                                 fixedlocationcoords = TRUE,
                                 what = NULL) {

  # checks
  if(!(inherits(y, "Track"))){
    stop("y must be a Track object\n")
  }
  if(!(is.logical(fixedlocationcoords) || length(fixedlocationcoords) != 1)){
    stop("fixedlocationcoords must be a logical value\n")
  }
  if(!(fixedlocationcoords && "location" %in% colnames(y@data))){
    stop("y does not contain a variable location\n")
  }

  # define an index for non gaps
  nongapsindex <- which(y@data$location != 0)

  # define the points to extract from
  if(!fixedlocationcoords){
    ypts <- y@sp[nongapsindex]
  }else{
    uniquelocationindex <- which(!duplicated(y@data$location))
    ypts <- sp::SpatialPointsDataFrame(coords = y@sp[uniquelocationindex],
                                       data = data.frame(location = y@data$location[uniquelocationindex]),
                                       proj4string = sp::CRS(sp::proj4string(y)))
  }

  # define the variables to extract
  if(is.null(what)){
    what <- seq_len(ncol(x@data))
  }

  # extract the values for all locations
  res <- sp::over(x = ypts, y = x, fn = fn, returnList = FALSE, ...)[, what]

  if(fixedlocationcoords){

    # add res to ypts
    ypts$res <- res

    # merge the values to currenttrack@data and keep only res
    dplyr::left_join(x = y@data, y = ypts@data, by = "location")["res"]

  }else{

    # return res
    res

  }

}
