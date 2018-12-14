#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import raster
NULL

#' Imputes gaps in a \code{\link[trajectories]{Track}} object.
#'
#' \code{fillGapTrackNight} imputes missing values in a
#' \code{\link[trajectories]{Track}} object. Gaps are filled
#' if their duration is $\le$ a user specified duration threshold
#' and if the distance between the spatial position of the last data
#' value before the gap and the spatial position of the first data
#' value after the gap is $\le$ a user specified distance threshold.
#' In contrast to \code{\link{fillGapTrack}}, \code{fillGapTrackNight}
#' considers only values in a specified time interval of the day (e.g.
#' during night).
#'
#' It has to be paid
#' attention to the fact that \code{maxduration} should be adapted to
#' the time interval specified by \code{night}. For example, if one specifies
#' the time interval between 0 and 4 o' clock as night and wants to allow
#' a \code{maxduration} of four days (assuming that there may be gaps for four
#' following nights, but if there is no gap in the fifth night, then the gap is
#' filled), then one must specifiy \code{maxduration} as
#' \code{4*24*60*60 + (24 - nightduration)*60*60}, whereby \code{nightduration}
#' is the duration of the time interval specified as night in hours.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object with a
#' boolean column \code{gap} in \code{currenttrack@data}. Data values
#' have to be regularly spaced (may be achieved for example with
#' \code{\link{reorganizeTracks}}).
#' @param maxduration A numerical value representing the maximum
#' allowed duration of a gap that is filled [s]. It has to be paid
#' attention to the fact that \code{maxduration} should be adapted to
#' the time interval specified by \code{night}. See the details section.
#' @param maxdistance A numerical value representing the maximum
#' allowed distance between the spatial position of the last data
#' value before a gap and the spatial position of the first data
#' value after a gap that is filled [m].
#' @param timeinterval A numerical value reperesenting the duration
#' of a time interval represented by one data value of
#' \code{currenttrack} [s].
#' @param night An integer vector with two elements:
#' \enumerate{
#'   \item The first element specifies the start hour of the night, e.g. \code{0}
#'   for 0 o'clock.
#'   \item The first element specifies the start hour of the night, e.g. \code{4}
#'   for 4 o'clock.
#' }
#' @return The input \code{\link[trajectories]{Track}} object with filled
#' gaps.
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{extractClutersBuffer}},
#' \code{\link{redefineIndices}},
#' \code{\link{fillGapTracksNight}}, \code{\link{fillGapTrack}}.
#' @examples #
#' @export
fillGapTrackNight <- function(currenttrack, maxduration, maxdistance, timeinterval, night = c(16, 20)){

  # get the input CRS of the track
  inputcrs <- proj4string(currenttrack)

  # classify data values as night or day values
  currentnighttrack <- classifyNightTrack(currenttrack, night = c(16, 20))

  # get the row indices of night values in currentnighttrack
  currentnighttrackindex <- which(attr(currentnighttrack, "night") == TRUE)

  # delete all day values in currentnighttrack@data
  currentnighttrackdata <- currentnighttrack@data
  currentnighttrackdata <- currentnighttrackdata[currentnighttrackindex,]

  # identify blocks of representing gaps
  blocksgaps1 <- identifyBlocksVariable(currentnighttrackdata, variable = "gap", value = TRUE)

  # convert the block indices to respective indices of all values
  blocksgaps1 <- apply(blocksgaps1, 2, function(x) currentnighttrackindex[x])

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
