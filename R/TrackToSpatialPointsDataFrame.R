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
#' a user specified coordinate reference system if specified.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object.
#' @param toproject A logical value indicating if the Track object
#' should be projected to a coordinate reference system as defined
#' by \code{crs} (\code{project = TRUE}) or not
#' (\code{project = FALSE}).
#' @param crs A character string describing a projection and datum
#' in the \code{PROJ.4} format (see \code{\link[rgdal]{projInfo}}).
#' @return A \code{\link[sp]{SpatialPointsDataFrame}} object
#' containing the information of the slot \code{sp} and \code{data}
#' of the corresponding \code{\link[trajectories]{Track}} object.
#' @seealso
#' @examples #
#' @export
TrackToSpatialPointsDataFrame <- function(currenttrack, toproject = TRUE, crs = "+proj=utm +zone=46 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"){

  # get the Track data
  trsdf <- currenttrack@data

  # convert the track to SpatialPointsDataFrame
  trsSP <- as(as(currenttrack, "SpatialLines"), "SpatialPoints")

  # transform trsSP to the specified crs
  if(toproject == TRUE){
    trsSP <-
    spTransform(
      trsSP,
      CRS(
        crs
      )
    )
  }

  # convert trsSP to a SpatialPointsDataFrame
  SpatialPointsDataFrame(trsSP, trsdf, match.ID = FALSE)

}
