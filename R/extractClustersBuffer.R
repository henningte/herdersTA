#' @importFrom Rdpack reprompt
#' @import spacetime
#' @import lubridate
#' @import trajectories
#' @import rgdal
#' @import sp
#' @import rgeos
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
#' \code{\link{reorganizeTracks}}.
#' @param radius A numerical value representing the radius of the
#' buffers computed around each point [m] which are used for
#' clustering values to locations. Default is \code{radius = 200} [m].
#' @return An integer vector with the same length as the number of points in
#' \code{trsSP} indicating to which cluster each point is assigned. Cluster
#' indices are not ordered, however points of \code{trsSP} representing gaps
#' (as indicated by \code{trsSP@data$gap}) get the cluster index \code{0}.
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{redefineIndices}},
#' \code{\link{fillGapTrack}}, \code{\link{fillGapTracks}},
#' \code{\link{locationsTrack}}.
#' @examples #
#' @export
extractClustersBuffer <- function(trsSP, radius = 200){

  # indices with points that have not been assigned to a cluster yet
  leftover <- c(1:length(trsSP))
  leftoverold <- leftover
  first <- TRUE

  while(length(leftover) != 0){

    # subset points for buffering (1%)
    indselect <- leftover[seq(1, length(leftover), length.out = length(leftover)*0.01)]

    # compute buffer
    buffer.trs <- gBuffer(trsSP[indselect,], width = radius, quadsegs = 25)

    # get intersecting points
    a.new <- over(trsSP, disaggregate(buffer.trs))

    if(first == TRUE){
      # get intersecting points
      buffer.trs.tot <- buffer.trs
      initiallocationids <- a.new

      first = FALSE
    }

    # define points that were inside a buffer at any loop run
    initiallocationids[which(is.na(initiallocationids))] <- a.new[which(is.na(initiallocationids))]

    # define total buffer range
    buffer.trs.tot <- gUnion(buffer.trs, buffer.trs.tot)

    # subset leftover
    if(length(which(is.na(initiallocationids))) != 0){
      leftover <- leftoverold[which(is.na(initiallocationids))]
    }else{
      leftover = NULL

      initiallocationids <- over(trsSP, disaggregate(buffer.trs.tot))
    }

  }

  # set location of gaps to 0
  initiallocationids[which(trsSP@data$gap == TRUE & trsSP@data$filled == FALSE)] <- 0

  # return result
  return(initiallocationids)

}
