#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import rgdal
#' @import sp
NULL

#' Converts a \code{\link[trajectories]{Track}} object to a \code{\link[sp]{SpatialPointsDataFrame}}.
#'
#' \code{TrackToSpatialPointsDataFrame} converts a
#' \code{\link[trajectories]{Track}} object to a
#' \code{\link[sp]{SpatialPointsDataFrame}} and projects it to
#' "+proj=utm +zone=46 +datum=WGS84 +units=m +no_defs
#' +ellps=WGS84 +towgs84=0,0,0".
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object.
#' @return A \code{\link[sp]{SpatialPointsDataFrame}} object
#' containing the information of the slot \code{sp} and \code{data}
#' of the corresponding \code{\link[trajectories]{Track}} object.
#' @seealso
#' @examples #
#' @export
TrackToSpatialPointsDataFrame <- function(currenttrack){

  # get the Track data
  trsdf <- currenttrack@data

  # convert the track to SpatialPointsDataFrame and transform to UTM
  trsSP <- as(as(currenttrack, "SpatialLines"), "SpatialPoints")
  trsSP <-
    spTransform(
      trsSP,
      CRS(
        "+proj=utm +zone=46 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
      )
    )

  # convert trsSP to a SpatialPointsDataFrame
  SpatialPointsDataFrame(trsSP, trsdf, match.ID = FALSE)

}
