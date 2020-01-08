#' @importFrom doParallel registerDoParallel
#' @importFrom parallel clusterExport makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
NULL

#' Extracts corresponding raster values for GPS tracks.
#'
#' \code{extractRasterTrack} is the \code{\link[raster:extract]{extract}} method
#' in order to extract values from \code{\link[raster:raster]{Raster*}} objects
#' for an object of class \code{\link[trajectories:Track-class]{TracksCollection}}. Different
#' options are available.
#'
#' @param x A Raster* object. \code{x} must either have one layer or
#' the number of layers of \code{x} has to be the same as the number of
#' data values in \code{y}. If a multi-layer object is provided, it
#' is automatically assumed that this is the case.
#' @param y A \code{\link[trajectories:Track-class]{TracksCollection}} object.
#' Depending on other parameters set, there may be certain variables required.
#' @param datetime A POSIXct vector with a date (day) for each layer in \code{x}.
#' @param method A character value. \code{"simple"} or \code{"bilinear"}. If \code{"simple"}
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
#' @param fixedlocationcoords A logical value indicating if for each location in the
#' \code{Track} objects of \code{y}
#' the same position is assumed for all data values. In this case, computation
#' can be speed up by setting \code{location = TRUE} and the function uses
#' simply the coordinates of the first value for each location.
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel:clusterApply]{clusterCall}}.
#' @return A vector with a value for each data value of \code{currenttrack}.
#' If a value of \code{raster} cannot be assigned to a respective data value,
#' \code{NA} is returned for the respective data value.
#' @seealso
#' \code{\link{extractPolygonsTracks}},
#' \code{\link{extractRasterTrack}}.
#' @examples #
#' @export
extractRasterTracks <- function(x,
                                y,
                                datetime,
                                method = "simple",
                                buffer = 0,
                                small = TRUE,
                                fun = NULL,
                                na.rm = TRUE,
                                fixedlocationcoords = FALSE,
                                cores = 1,
                                clcall = NULL) {

  # checks
  if(!(inherits(y, "TracksCollection"))){
    stop("y must be a TracksCollection object\n")
  }

  # avoid no visible bindings message
  tracks <- NULL

  # set up cluster
  cl <- parallel::makeCluster(cores, outfile="", type = "PSOCK")
  doParallel::registerDoParallel(cl)
  if(is.null(clcall) == FALSE){
    parallel::clusterCall(cl, clcall)
  }
  on.exit(expr = parallel::stopCluster(cl))

  # merge the Track objects of each Tracks object
  foreach::foreach(tracks = seq_along(y@tracksCollection), .packages = c("trajectories", "sp", "dplyr", "raster"), .export = c("extractRasterTrack"))%dopar%{

    res <- extractRasterTrack(x = x,
                              y = y@tracksCollection[[tracks]]@tracks[[1]],
                              datetime = datetime,
                              method = method,
                              buffer = buffer,
                              small = small,
                              fun = fun,
                              na.rm = na.rm,
                              fixedlocationcoords = fixedlocationcoords)
    res$household <- tracks
    res

  }

}
