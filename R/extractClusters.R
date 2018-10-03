#'@importFrom Rdpack reprompt
#'@import spatstat
#'@import sp
NULL

#' Extracts clusters based on point counts in a spatial grid.
#'
#' \code{extractClusters} extract clusters using the function
#' \code{\link{pointsPerQuad}}.
#'
#' @param currenttracks A \code{\link[trajectories]{Tracks}} object.
#' @param targetQsize Numerical value representing the target size of the
#' quadrats to create.
#' @param threshold An integer value specifying the minimum number of
#' points within a grid cell needed in order to consider these points
#' as cluster.
#' @return A \code{\link[sp]{SpatialGridDataFrame}} with the cells
#' determined as clusters.
#' @seealso \code{\link{qTopology}}, \code{\link{pointsPerQuad}},
#' \code{\link{searchNextVisit}}, \code{\link{findStarts}},
#' \code{\link{clusterOrder}}.
#' @examples #
#' @export
extractClusters <- function(currenttracks, targetQsize = 800, threshold = 20){

    # count the number of points of the Tracks object within the spatial grid cells
    trsGrid <- pointsPerQuad(currenttracks, targetQsize)

    # convert to SpatialPolygonsDataFrame
    trsGridSPDF <- as(trsGrid, "SpatialPolygonsDataFrame")

    # get cells with count > threshold
    trsGridSPDF[which(trsGridSPDF$count > threshold), ]

  }
