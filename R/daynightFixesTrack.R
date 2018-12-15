#'@importFrom Rdpack reprompt
#'@import trajectories
#'@import sp
NULL

#' Classifies sub-daily values of GPS tracks.
#'
#' \code{daynightFixesTrack} classifies the values of a
#' \code{\link[trajectories:Track-class]{Track}} object in two classes on a sub-daily basis
#' by defining threshold hour values (for example in order to classify values as
#' recorded during daytime and nighttime).
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object.
#' @param tstart A numeric value indicating the start of the time interval
#' [h, values between 0 and 24]. \code{tstart} has to be smaller than
#' \code{tend}.
#' @param tend A numeric value indicating the end of the time interval
#' [h, values between 0 and 24]. \code{tend} has to be larger than
#' \code{tstart}.
#' @return a \code{\link[sp]{SpatialPointsDataFrame}} containing all data
#' of the slot \code{data} of the input \code{\link[trajectories:Track-class]{Track}}
#' object and a column \code{night} indicating if a data point is within
#' the specified time interval (\code{night = 1}) or not
#' (\code{night = 0}).
#' @seealso \code{\link{downByDrop}}, \code{\link{downByDropTrs}},
#' \code{\link{getNightTrack}}, \code{\link{getNightTrs}},
#' \code{\link{dayNightFixesTracks}},
#' \code{\link{getNightConnectionsTrack}}.
#' @examples #
#' @export
daynightFixesTrack <- function (currenttrack,
                                tstart = 16,
                                tend = 20){

  #get indexes of fixes within time frame
  nightindexes <- which(hour(currenttrack@time) >= tstart & hour(currenttrack@time) < tend)

  # get SpatialPoints of all fixes during night
  nightfixes <- currenttrack@sp[nightindexes,]
  nightfixes <-
    SpatialPointsDataFrame(nightfixes, cbind(currenttrack@data[nightindexes,], data.frame(night = rep(
      1, length(nightfixes)
    ))))

  # get SpatialPoints of all other fixes
  if (length(nightfixes) > 0){
    dayfixes <- currenttrack@sp[-nightindexes,]
    dayfixes <-
      SpatialPointsDataFrame(dayfixes, cbind(currenttrack@data[-nightindexes,], data.frame(night = rep(
        0, length(dayfixes))))
      )
  }else{
    dayfixes <- currenttrack@sp
    dayfixes <-
      SpatialPointsDataFrame(dayfixes, cbind(currenttrack@data, data.frame(night = rep(
        0, length(dayfixes))))
      )
  }

  if(length(nightfixes) == 0){
    return(dayfixes)
  }else if (length(dayfixes) == 0){
    return(nightfixes)
  }else{
    allfixes <- rbind(dayfixes, nightfixes)
    return(allfixes)
  }

}
