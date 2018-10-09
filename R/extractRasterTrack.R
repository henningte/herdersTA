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
#' @seealso \code{\link{assignFixedTenDayInterval}},
#' \code{\link{extractRasterTracks}}.
#' @examples #
#' @export
extractRasterTrack <- function(currenttrack, raster, timedate, resolution){

  # adjust the temporal resolution of currenttrack$time to timedate accoriding to resolution
  switch(resolution,
         days = {

           # aggregate to daily resoltuion
           currenttracktime <- strftime(currenttrack$time, "%Y-%m-%d")

           # define an index for each day
           indexcurrenttracktime <- as.factor(currenttracktime)
         },
         fixedtendays = {

           # aggregate to daily resoltuion
           currenttracktime <- strftime(currenttrack$time, "%Y-%m-%d")

           # get indices for each ten-day interval
           indexcurrenttracktime <- assignFixedTenDayInterval(as.POSIXct(currenttracktime), startnew = FALSE)

         },
         movingwindowtendays = {

           # aggregate to daily resolution
           currenttracktime <- as.POSIXct(strftime(currenttrack$time, "%Y-%m-%d"))

           # get indices for each ten-day interval
           indexaggregatedtrackvalues  <-lapply(seq_along(unique(currenttracktime)), function(x){

             which(currenttracktime >= as.POSIXct(unique(currenttracktime)[[x]]) & currenttracktime <= as.POSIXct(unique(currenttracktime)[[x]]) + 10*24*60*60)

           })
           names(indexaggregatedtrackvalues) <- unique(currenttracktime)

         },
         months = {

           # aggregate to monthly resolution
           currenttracktime <- strftime(currenttrack$time, "%Y-%m")

           # get indices for each month
           indexcurrenttracktime <- as.factor(currenttracktime)

         })

  # define an list collecting the data value indices of crrenttrack for each aggregated time value
  if(resolution != "movingwindowtendays"){

    indexaggregatedtrackvalues <- lapply(unique(indexcurrenttracktime), function(x){
      currenttracktime[indexcurrenttracktime == x]
    })
    names(indexaggregatedtrackvalues) <- unique(currenttracktime)

  }

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

} # not tested yet for fixed ten-day interval resolution, moving window ten-day interval resolution, monthly resolution

