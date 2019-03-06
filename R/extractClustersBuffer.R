#' @importFrom Rdpack reprompt
#' @import trajectories
#' @importFrom sp SpatialPoints coordinates
NULL

#' Identifies clusters of points of GPS tracks.
#'
#' \code{extractClustersBuffer} identifies clusters of points in GPS
#' tracks based on their spatial proximity. This is done by applying
#' density based clustering on all points within a specific daily time
#' interval (e.g. during night). Daytime data values are assigned to
#' the respective location id (1) if the next value after and prior the
#' daytime values (i.e. the last value of the previous night and the
#' first value of the following night) are identical (i.e. are defined
#' as gaps if both values are gaps or to a location id otherwise), then
#' (2) if the next value of the previous night is not a gap to the
#' respective location, and then (3) if the next value of the following
#' night is not a gap to the respective location. Therefore, results
#' reflect a daily resolution with respect to different locations.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}.
#' \code{currenttrack} must have a logical variable \code{gap} as created by
#' \code{\link{reorganizeTracks}} and an attribute \code{night} as
#' created by \code{\link{classifyNightTrack}}.
#' @param radius A numerical value representing the radius of the
#' buffers computed around each point [m] which are used for
#' clustering values to locations. Default is \code{radius = 200} [m].
#' @return An integer vector with the same length as the number of points in
#' \code{currenttrack} indicating to which cluster each point is assigned. Cluster
#' indices are not ordered, however points of \code{trsSP} representing gaps
#' (as indicated by \code{currenttrack$gap}) get the cluster index \code{0}.
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{redefineIndices}},
#' \code{\link{fillGapTrack}}, \code{\link{fillGapTracks}},
#' \code{\link{locationsTrack}}.
#' @examples #
#' @export
extractClustersBuffer <- function(currenttrack, radius = 800){

  # use density based clustering in order to identify locations
  locations_night <- dbscan(x = coordinates(currenttrack@sp)[attributes(currenttrack)$night == TRUE,], eps = radius, minPts = 5)

  # insert the location ids into a vector for all data values
  locations_night_all <- rep(0, nrow(currenttrack@data))
  locations_night_all[attributes(currenttrack)$night == TRUE] <- locations_night$cluster

  # set the location id of gaps to 0
  locations_night_all[currenttrack$gap == TRUE] <- 0

  # return the locations_night_all ids
  return(locations_night_all)

}
