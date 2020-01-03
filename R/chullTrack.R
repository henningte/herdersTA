#' @importFrom grDevices chull
#' @importFrom raster crs
#' @importFrom sp SpatialPolygons Polygons proj4string
NULL

#' Extracts the convex hull from a Track object.
#'
#' \code{chullTrack} extracts the convex hull from a
#' \code{\link[trajectories:Track-class]{Track}} object and returns it
#' as \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}} object.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}.
#' @return A \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}} object
#' with coordinates corresponding to coordinates of points of the convex hull.
#' @seealso .
#' @examples #
#' @export
chullTrack <- function(currenttrack){

  # checks
  if(!(inherits(currenttrack, "Track"))){
    stop("currenttrack must be a Track object\n")
  }

  # extract the coordinates
  xcoords <- currenttrack@sp@coords

  # compute the convex hull
  index <- grDevices::chull(x = xcoords)

  # extract the corresponding coordinates
  ch <- sp::SpatialPolygons(Srl = list(sp::Polygons(srl = list(sp::Polygon(coords = xcoords[index,])), ID = integer(1))))

  # add the crs information
  raster::crs(ch) <- sp::proj4string(currenttrack)
  ch

}
