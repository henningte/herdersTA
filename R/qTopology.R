#' Defines the topology of a spatial grid.
#'
#' \code{qTopology} determines topology for grid and quadratcount, i.e.
#' size in x and y such that the window of the ppp can be split in an
#' even number of quadrats, approximating a certain target edge length,
#' e.g. 1000 m.
#'
#' This function is used by function \code{\link{pointsPerQuad}}
#' to create the cells in which to count the number of points.
#'
#' @param ppp A \code{\link[spatstat]{ppp}} (point pattern dataset) object.
#' @param targetQsize Numerical value representing the target size of the
#' quadrats to create.
#' @return A list containing the size and number of cells in x and y.
#' @seealso \code{\link{pointsPerQuad}}, \code{\link{extractClusters}},
#' \code{\link{searchNextVisit}}, \code{\link{findStarts}},
#' \code{\link{clusterOrder}}.
#' @examples #
#' @export
qTopology <- function(ppp,
                      targetQsize) {

  #determine next number of quadrats in x and y direction, by simple rounding
  nqx <- round((ppp$window$xrange[2] - ppp$window$xrange[1]) / targetQsize)
  nqy <- round((ppp$window$yrange[2] - ppp$window$yrange[1]) / targetQsize)

  #determine quadrat size using rounded number of cells
  qsizeX <- (ppp$window$xrange[2] - ppp$window$xrange[1]) / nqx
  qsizeY <- (ppp$window$yrange[2] - ppp$window$yrange[1]) / nqy

  # return the results in a list
  list(
    qsizeX = qsizeX,
    qsizeY = qsizeY,
    nqx = nqx,
    nqy = nqy
  )

}
