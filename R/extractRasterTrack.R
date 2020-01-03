#' @importFrom raster extract nlayers
#' @importFrom sp SpatialPointsDataFrame CRS proj4string
#' @importFrom lubridate as_date
#' @importFrom data.table rbindlist
#' @importFrom dplyr left_join
NULL

#' Extracts corresponding raster values for GPS tracks.
#'
#' \code{extractRasterTrack} is the \code{\link[raster:extract]{extract}} method
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
#' @param character. \code{"simple"} or \code{"bilinear"}. If \code{"simple"}
#' values for the cell a point falls in are returned. If \code{"bilinear"} the
#' returned values are interpolated from the values of the four nearest raster
#' cells.
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
#' @param fixedlocationcoords A logical value indicating if for each location in \code{y}
#' the same position is assumed for all data values. In this case, computation
#' can be speed up by setting \code{location = TRUE} and the function uses
#' simply the coordinates of the first value for each location.
#' @return A vector with a value for each data value of \code{currenttrack}.
#' If a value of \code{raster} cannot be assigned to a respective data value,
#' \code{NA} is returned for the respective data value.
#' @seealso \code{\link{assignFixedTenDayInterval}},
#' \code{\link{extractRasterTracks}}.
#' @examples #
#' @export
extractRasterTrack <- function(x,
                               y,
                               datetime = NULL,
                               method = "simple",
                               buffer = 0,
                               small = TRUE,
                               fun = NULL,
                               na.rm = TRUE,
                               fixedlocationcoords = FALSE
) {

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

  # number of layers in x
  xnlayers <- raster::nlayers(x)

  # check if xnlayers == 1 || xnlayers == nrow(y@data)
  if(!(length(xnlayers) == 1 || xnlayers == nrow(y@data))){
    stop("x must contain either one layer or the number of layers of x must match the number of values in y\n")
  }

  # define an index for non gaps
  nongapsindex <- which(y@data$location != 0)

  # define the points to extract from
  if(!fixedlocationcoords){
    ypts <- y@sp[nongapsindex]
  }else{
    uniquelocationindex <- which(!duplicated(y@data$location) & y@data$location != 0)
    ypts <- sp::SpatialPointsDataFrame(coords = y@sp[uniquelocationindex], data = data.frame(location = y@data$location[uniquelocationindex]), proj4string = sp::CRS(sp::proj4string(y)))
  }

  # define layer
  layer <- 1

  if(xnlayers == 1){

    # extract the values for all locations
    res <- raster::extract(x = x,
                           y = ypts,
                           method = method,
                           buffer = buffer,
                           na.rm = na.rm,
                           layer = layer,
                           nl = xnlayers,
                           fun = fun,
                           df = TRUE)[,2]

  }else{

    if(!fixedlocationcoords) stop("fixedlocationcoords = FALSE is not supported if a multilayer raster object is provided as x\n")

    # checkout the overlapping time intervals
    tracktime <- lubridate::as_date(as.POSIXct(y@time))
    dateinterval <- lubridate::as_date(max(tracktime[1], datetime[1]):min(c(datetime[length(datetime)], tracktime[length(tracktime)])))

    # create a clipped version of x
    trackdatetimeindex <- which(tracktime %in% dateinterval)
    xclipped <- x
    xnlayersclipped <- raster::nlayers(xclipped)

    # create a data.frame to store the results in
    res <- data.frame(date = rep(tracktime, length(uniquelocationindex)),
                      location = rep(y@data$location[uniquelocationindex], each = length(tracktime)),
                      valpres = rep(tracktime %in% dateinterval, length(uniquelocationindex)),
                      res = NA)

    # extract the values for all locations (and stack them to a column)
    cellnumbers <- raster::extract(x = xclipped[[1]], y = ypts, method = method, buffer = buffer, na.rm = na.rm, layer = layer, nl = xnlayersclipped, fun = NULL, df = FALSE, cellnumbers = TRUE)
    cellnumbers <- data.table::rbindlist(lapply(seq_along(cellnumbers), function(z){
      cellnumbers <- data.frame(location = ypts$location[z],
                                matrix(cellnumbers[[z]],
                                       ncol = 2),
                                stringsAsFactors = FALSE)
    }))

    xres <- raster::extract(x = xclipped,
                            y = unlist(cellnumbers[,2]),
                            layer = layer,
                            nl = xnlayersclipped,
                            df = TRUE)[,-1]
    if(!(tracktime[1] %in% datetime)){
      xres[,1] <- rep(NA, nrow(xres))
    }
    res$res[res$valpres] <- as.vector(t(apply(xres, 2, function(z){
      tapply(z, cellnumbers[,1], function(q) fun(q, na.rm = na.rm))
    })))

  }

  if(fixedlocationcoords && xnlayers == 1){

    # add res to ypts
    ypts$res <- res

    # merge the values to currenttrack@data and keep only res
    dplyr::left_join(x = y@data, y = ypts@data, by = "location")["res"]

  }else{

    res[,c(2, 1, 4)]

  }

}
