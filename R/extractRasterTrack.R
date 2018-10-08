#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import raster
NULL

#' Extracts corresponding raster values for GPS tracks.
#'
#' \code{extractRasterTrack} extracts values from raster based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) that correspond the the respective
#' position and time of a \code{\link[trajectories]{Track}} object.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object that
#' has a column \code{time} containing the time information of the data
#' values.
#' @param raster raster based time series (\code{RasterBrick} or
#' \code{RasterStack} object, see: \code{\link[raster]{Raster-class}}).
#' @param timedate A \code{POSIXct} vector with values for each layer of
#' \code{raster}.
#' @param resolution A character value indicating the temporal resolution
#' of \code{raster} and \code{timedate}, respectively. One of \code{"days"},
#' \code{"fixedtendays"}, \code{"movingwindowtendays"} and \code{"months"}.
#' @return A vector with a value for each data value of \code{currenttrack}.
#' If a value of \code{raster} cannot be assigned to a respective data value,
#' \code{NA} is returned for the respective data value.
#' @seealso .
#' @examples #
#' @export
extractRasterTrack <- function(currenttrack, raster, timedate, resolution){

  # adjust the temporal resolution of currenttrack$time to timedate accoriding to resolution
  if(resolution %in% c("days", "fixedtendays", "movingwindowtendays")){
    currenttracktime <- strftime(currenttrack$time, "%Y-%m-%d")
  }else{
    currenttracktime <- strftime(currenttrack$time, "%Y-%m")
  }

  # define an list collecting the data value indices of crrenttrack for each aggregated time value
  indexaggregatedtrackvalues <- lapply(unique(currenttracktime), function(x){
    which(currenttracktime == x)
  })
  names(indexaggregatedtrackvalues) <- unique(currenttracktime)

  # define an index assigning raster layers to elements of indexrasterlayertrackvalues
  indexrasterlayertrackvalues <- which(timedate %in% names(indexaggregatedtrackvalues))

  # extract the values
  do.call(c, lapply(seq_along(indexaggregatedtrackvalues), function(x){

    print(x)

    # define an index for the current data values of currenttrack
    index <- indexaggregatedtrackvalues[[x]]

    # extract the respective data values of currenttrack and convert it to a SpatialPointsDataFrame and project it
    currenttracksubset <- TrackToSpatialPointsDataFrame(Track(track = STIDF(sp = currenttrack@sp[index], time = as.POSIXct(currenttrack$time[index]), data = currenttrack@data[index,], endTime = currenttrack$time[index])))

    # extract the respective values
    extract(raster[[indexrasterlayertrackvalues[x]]], currenttracksubset)

  }))

}

