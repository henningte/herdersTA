#' @importFrom trajectories TracksCollection Tracks
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel clusterExport makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
NULL

#' Equalise Track Coordinates to Locations.
#'
#' \code{equaliseLocationsCoordinatesTracks} computes the median
#' coordinates (longitude, latitude) for each location of all tracks
#' (object of class \code{\link[trajectories:Track-class]{Track}})
#' of an object of class \code{\link[trajectories:Track-class]{TracksCollection}}
#' and assigns these coordinates to matching data values in each track.
#' During this procedure, filled values are not considered in order to
#' not influence the median coordinates by the gap filling procedure.
#' The function uses \code{\link{equaliseLocationsCoordinatesTrack}}.
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{TracksCollection}}
#' object with one \code{\link[trajectories:Track-class]{Track}} object per
#' \code{\link[trajectories:Track-class]{Tracks}} object that has a variable
#' \code{location} and a variable \code{filled} in the data slot.
#' \code{location} must be numeric with an integer value for each unique location.
#' Gaps must have the location \code{0}.
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel:clusterApply]{clusterCall}}.
#' @return \code{currenttracks} with the same longitude and latitude values for
#' each location for all \code{\link[trajectories:Track-class]{Track}} objetcs.
#' @seealso \code{\link{locationsTrack}}.
#' @examples #
#' @export
equaliseLocationsCoordinatesTracks <- function(currenttracks,
                                               cores = 1,
                                               clcall = NULL) {

  # checks
  if(!(inherits(currenttracks, "TracksCollection"))){
    stop("currenttracks must be a TracksCollection\n")
  }

  # avoid no visible bindings message
  x <- NULL

  # extract the names
  currenttracksnames <- names(currenttracks@tracksCollection)

  # set up cluster
  cl <- parallel::makeCluster(cores, outfile="", type = "PSOCK")
  doParallel::registerDoParallel(cl)
  if(is.null(clcall) == F){
    parallel::clusterCall(cl, clcall)
  }
  on.exit(expr = parallel::stopCluster(cl))

  # adjust the coordinates
  newcurrenttracks <- trajectories::TracksCollection(
    foreach::foreach(x = currenttracks@tracksCollection,
                     .packages = c("trajectories", "sp", "data.table"),
                     .export = c("equaliseLocationsCoordinatesTrack"))%dopar%{
    trajectories::Tracks(list(equaliseLocationsCoordinatesTrack(currenttrack = x@tracks[[1]])))
  })

  # restore the names
  names(newcurrenttracks@tracksCollection) <- currenttracksnames

  # return currenttrack
  return(newcurrenttracks)

}
