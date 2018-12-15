#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import raster
NULL

#' Imputes gaps in a \code{\link[trajectories:Track-class]{Track}} object.
#'
#' \code{fillGapTrack} imputes missing values in a
#' \code{\link[trajectories:Track-class]{Track}} object. Gaps are filled
#' if their duration is $\le$ a user specified duration threshold
#' and if the distance between the spatial position of the last data
#' value before the gap and the spatial position of the first data
#' value after the gap is $\le$ a user specified distance threshold.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object with a
#' boolean column \code{gap} in \code{currenttrack@data}. Data values
#' have to be regularly spaced (may be achieved for example with
#' \code{\link{reorganizeTracks}}).
#' @param maxduration A numerical value representing the maximum
#' allowed duration of a gap that is filled [s].
#' @param maxdistance A numerical value representing the maximum
#' allowed distance between the spatial position of the last data
#' value before a gap and the spatial position of the first data
#' value after a gap that is filled [m].
#' @param timeinterval A numerical value reperesenting the duration
#' of a time interval represented by one data value of
#' \code{currenttrack} [s].
#' @return The input \code{\link[trajectories:Track-class]{Track}} object with filled
#' gaps.
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{extractClutersBuffer}},
#' \code{\link{redefineIndices}},
#' \code{\link{fillGapTracks}}, \code{\link{locationsTrack}}.
#' @examples #
#' @export
fillGapTrack <- function(currenttrack, maxduration, maxdistance, timeinterval){

  # get the input CRS of the track
  inputcrs <- proj4string(currenttrack)

  # identify blocks of representing gaps
  blocksgaps1 <- identifyBlocksVariable(currenttrack, variable = "gap", value = TRUE)

  # test if blocksgaps1 == NULL
  if(is.null(blocksgaps1)){
    currenttrack@data$filled = FALSE
    return(currenttrack)
  }

  # remove the first and the last gap (cannot be filled)
  if(1 %in% blocksgaps1[,1]){
    blocksgaps1 <- blocksgaps1[-which(blocksgaps1[,1] == 1),]
  }
  if(nrow(currenttrack@data) %in% blocksgaps1[,2]){
    blocksgaps1 <- blocksgaps1[-which(blocksgaps1[,2] == nrow(currenttrack@data)),]
  }

  # get duration of gaps
  discardgaps <-
    apply(blocksgaps1, 1, function(x){
      if(length(x[1]:x[2])*timeinterval > maxduration){
        0
      }else{
        1
      }
    })

  # discard gaps > maxduration
  if(length(which(discardgaps == 0)) > 0){
    blocksgaps1 <- blocksgaps1[-which(discardgaps == 0),]
  }

  # test if blocksgaps1 == NULL
  if(is.null(blocksgaps1)){
    currenttrack@data$filled = FALSE
    return(currenttrack)
  }

  # get distance between points adjacent to each gap block
  discardgaps <-
    apply(blocksgaps1, 1, function(x){

      block.dist <- pointDistance(c(currenttrack@data$lon[x[1]-1], currenttrack@data$lat[x[1]-1]), c(currenttrack@data$lon[x[2]+1], currenttrack@data$lat[x[2]+1]), lonlat = TRUE)
      if(is.na(block.dist)){
        0
      } else{
        if(block.dist > maxdistance){
          0
        }else{
          1
        }
      }

    })

  # discard gaps with differing locations between points adjacent to each gap block
  if(length(which(discardgaps == 0)) > 0){
    blocksgaps1 <- blocksgaps1[-which(discardgaps == 0),]
  }

  # test if blocksgaps1 == NULL
  if(is.null(blocksgaps1)){
    currenttrack@data$filled = FALSE
    return(currenttrack)
  }

  # fill gaps
  currenttrack1 <- currenttrack
  currenttrack@data[as.vector(unlist(apply(blocksgaps1, 1, function(x){x[1]:x[2]}))),] <-
    do.call(rbind, apply(blocksgaps1, 1, function(x){
      do.call(rbind, replicate(length(x[1]:x[2]), currenttrack@data[x[1]-1,], simplify = FALSE))
    }))
  currenttrack@data$time[as.vector(unlist(apply(blocksgaps1, 1, function(x){x[1]:x[2]})))] <-
    currenttrack1@data$time[as.vector(unlist(apply(blocksgaps1, 1, function(x){x[1]:x[2]})))]

  # create a variable to classify filled gaps
  currenttrack@data$filled <- rep(FALSE, nrow(currenttrack@data))
  currenttrack@data$filled[as.vector(unlist(apply(blocksgaps1, 1, function(x){x[1]:x[2]})))] <- TRUE

  # recreate currenttrack as Track object
  currenttrack <- Track(STIDF(sp = SpatialPoints(cbind(currenttrack@data$lon, currenttrack@data$lat)), time = as.POSIXct(currenttrack@data$time) , data = currenttrack@data, endTime = as.POSIXct(currenttrack@data$time)))

  # set crs
  crs(currenttrack@sp) <- inputcrs

  # return result
  return(currenttrack)

}
