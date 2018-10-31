#' @importFrom Rdpack reprompt
#' @import spacetime
#' @import lubridate
#' @import trajectories
#' @import rgdal
#' @import sp
#' @import rgeos
#' @import dbscan
NULL

#' Identifies clusters of points of GPS tracks.
#'
#' \code{extractClustersBuffer} identifies clusters of points in GPS
#' tracks based on their spatial proximity. This is done by iteratively
#' assigning points to buffers around selected points and merging
#' intersecting buffers and point sets.
#'
#' @param trsSP A \code{\link[sp]{SpatialPointsDataFrame}} object
#' representing a \code{\link[trajectories]{Track}} object, i.e.
#' the result of the function
#' \code{\link{TrackToSpatialPointsDataFrame}}. \code{trsSP} must
#' have a logical variable \code{gap} as created by
#' \code{\link{reorganizeTracks}} and an attribute \code{night} as
#' created by \code{\link{classifyNightTrack}}.
#' @param radius A numerical value representing the radius of the
#' buffers computed around each point [m] which are used for
#' clustering values to locations. Default is \code{radius = 200} [m].
#' @param onlytimeinterval A logical value indicating if the extraction
#' of locations should in the first place only consider data values with
#' a corresponding value \code{TRUE} in \code{attributes(trsSP)$night}
#' (\code{onlytimeinterval = TRUE}) or all data values simultaneously
#' (\code{onlytimeinterval = FALSE}). If \code{onlytimeinterval = TRUE},
#' first all values relating to \code{attributes(trsSP)$night = TRUE} are
#' clustered and afterwards, all values relating to
#' \code{attributes(trsSP)$night = FALSE} are assigned to the next location
#' within a distance of \code{radius} or if there is no location within
#' a distance of \code{radius}, form a new location.
#' @return An integer vector with the same length as the number of points in
#' \code{trsSP} indicating to which cluster each point is assigned. Cluster
#' indices are not ordered, however points of \code{trsSP} representing gaps
#' (as indicated by \code{trsSP@data$gap}) get the cluster index \code{0}.
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{redefineIndices}},
#' \code{\link{fillGapTrack}}, \code{\link{fillGapTracks}},
#' \code{\link{locationsTrack}}.
#' @examples #
#' @export
extractClustersBuffer <- function(trsSP, radius = 200, onlytimeinterval = TRUE){

  # use density based clustering in order to identify locations
  b <- dbscan(x = trsSP@coords, eps = radius, minPts = 5)

  # set the location id for gaps to 0
  b$cluster[trsSP@data$gap == TRUE] <- 0

  # return the location ids
  return(b$cluster)

}
