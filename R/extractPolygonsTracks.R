#' @importFrom doParallel registerDoParallel
#' @importFrom parallel clusterExport makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
NULL

#' Extracts corresponding raster values for GPS tracks.
#'
#' \code{extractPolygonsTracks} is a function
#' in order to extract values from \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}} objects
#' for an object of class \code{\link[trajectories:Track-class]{TracksCollection}}.
#'
#' @param x A \code{\link[sp:SpatialPolygons]{SpatialPolygons}} object.
#' @param y A \code{\link[trajectories:Track-class]{TracksCollection}} object.
#' Depending on other parameters set, there may be certain variables required.
#' @param fn function to summarize the values (e.g. \code{mean}).
#' @param ... further arguments passed to \code{\link[sp:over]{over}}.
#' @param fixedlocationcoords A logical value indicating if for each location in \code{y}
#' the same position is assumed for all data values. In this case, computation
#' can be speed up by setting \code{location = TRUE} and the function uses
#' simply the coordinates of the first value for each location.
#' @param what A numeric vector specifying the column(s) of \code{x}
#' from which to retain values. I fset to \code{NULL}, values for all
#' columns will be extracted.
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel:clusterApply]{clusterCall}}.
#' @return A vector with a value for each data value of \code{y}.
#' If a value of \code{x} cannot be assigned to a respective data value,
#' \code{NA} is returned for the respective data value.
#' @seealso
#' \code{\link{extractRasterTrack}},
#' \code{\link{extractPolygonsTrack}}.
#' @examples #
#' @export
extractPolygonsTracks <- function(x,
                                  y,
                                  fn = NULL,
                                  ...,
                                  fixedlocationcoords = TRUE,
                                  what = NULL,
                                  cores,
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
  foreach::foreach(tracks = seq_along(y@tracksCollection),
                   .packages = c("trajectories", "sp", "dplyr"),
                   .export = c("extractPolygonsTrack"))%dopar%{

    extractPolygonsTrack(x = x,
                         y = y@tracksCollection[[tracks]]@tracks[[1]],
                         fn = fn, ...,
                         fixedlocationcoords = fixedlocationcoords,
                         what = what)

  }

}
