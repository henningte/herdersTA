#'@importFrom Rdpack reprompt
#'@import spatstat
#'@import sp
NULL

#' Counts the number of points in the cells of a spatial grid.
#'
#' \code{pointsPerQuad} determines the number of points per
#' cell (of a given size) in a \code{\link[trajectories]{Tracks}}
#' object.
#'
#' This function is used by function \code{\link{extractClusters}} to
#' count the number of points within cells before determining clusters
#' of points.
#'
#' @param currenttracks A \code{\link[trajectories]{Tracks}} object.
#' @param targetQsize Numerical value representing the target size of the
#' quadrats to create.
#' @return A \code{\link[sp]{SpatialGridDataFrame}} with quadrat cells
#' and the number of points within them.
#' @seealso \code{\link{qTopology}}, \code{\link{extractClusters}},
#' \code{\link{searchNextVisit}}, \code{\link{findStarts}},
#' \code{\link{clusterOrder}}.
#' @examples #
#' @export
pointsPerQuad <- function(currenttracks, targetQsize){

  # transform the Tracks object to UTM
  trsSP <- as(as(currenttracks, "SpatialLines"), "SpatialPoints")
  trsSPtrans <-
    spTransform(
      trsSP,
      CRS(
        "+proj=utm +zone=46 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
      )
    )

  # convert the Tracks object to a ppp
  trsPPP <- as(trsSPtrans, "ppp")

  # use function qTopology to determine dimensions and number of cells for grid and quadratcount
  qtopo <- qTopology(trsPPP, targetQsize)
  qsizeX <- qtopo$qsizeX
  qsizeY <- qtopo$qsizeY
  nqx <- qtopo$nqx
  nqy <- qtopo$nqy

  # create and return a SpatialGridDataFrame with number of counts of points per grid cell using quadratcount and with dimensions determined above
  SpatialGridDataFrame(
    GridTopology(
      c(
        trsPPP$window$xrange[1] + qsizeX / 2,
        trsPPP$window$yrange[1] + qsizeY / 2
      ),
      c(qsizeX, qsizeY),
      c(nqx, nqy)
    ),
    data.frame(count = as.numeric(matrix(
      as.numeric(quadratcount(trsPPP, nx = nqx, ny = nqy)), nqx, nqy, byrow =
        T
    ))),
    proj4string = proj4string(trsSPtrans)
  )

}
