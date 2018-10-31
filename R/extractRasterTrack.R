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
#' @param aggregation A character value indicating for which spatial
#' aggregation of data values of \code{currenttrack} values of  \code{raster}
#' should be extracted:
#' \describe{
#'   \item{\code{"raw"}}{Values of  \code{raster} will be extracted for the
#'   data values of the original \code{\link[trajectories]{Track}} object.
#'   This may take a long time depending on the value of \code{resolution}
#'   and the amount of values of \code{currentrack}.}
#'   \item{\code{"allvisits"}}{Values of  \code{raster} will be extracted values
#'   aggregated for each visit, whereby it is distinguished between long-term
#'   visits (campsites) and short-term visits.}
#' }
#' @return A vector with a value for each data value of \code{currenttrack}.
#' If a value of \code{raster} cannot be assigned to a respective data value,
#' \code{NA} is returned for the respective data value.
#' @seealso \code{\link{assignFixedTenDayInterval}},
#' \code{\link{extractRasterTracks}}.
#' @examples #
#' @export
extractRasterTrack <- function(currenttrack,
                               raster,
                               timedate,
                               resolution,
                               aggregation = "allvisits"){

  # extract the time information for currenttrack
  switch(resolution,
         days = {
           # aggregate to daily resoltuion
           currenttracktime <- strftime(currenttrack$time, "%Y-%m-%d")
         },
         fixedtendays = {
           # aggregate to daily resoltuion
           currenttracktime <- strftime(currenttrack$time, "%Y-%m-%d")
         },
         movingwindowtendays = {
           # aggregate to daily resolution
           currenttracktime <- as.POSIXct(strftime(x$time, "%Y-%m-%d"))
         },
         months = {
           # aggregate to monthly resolution
           currenttracktime <- strftime(currenttrack$time, "%Y-%m")
         }
  )

  # aggregate the location information
  if(aggregation == "allvisits"){
    aggregatedvisits <- extractCoordinatesVisitsTrack(currenttrack = currenttrack,
                                                      aggregated = FALSE)
    currenttrack$lon <- aggregatedvisits$lon
    currenttrack$lat <- aggregatedvisits$lat
    currenttrack$HEIGHT <- aggregatedvisits$alt
    currenttrack@sp@coords <- cbind(aggregatedvisits$lon, aggregatedvisits$lat)
  }


  # define a list collecting the data value indices of crrenttrack for each aggregated time value
  if(resolution != "movingwindowtendays"){

    # get an index assigning each value of currenttrack to a time interval according to resolution and extract the time information from currenttrack
    indexcurrenttracktime <- assignTimeInterval(currenttrack, resolution)

    indexaggregatedtrackvalues <- lapply(unique(indexcurrenttracktime), function(x){
      seq_along(currenttracktime)[indexcurrenttracktime == x]
    })
    names(indexaggregatedtrackvalues) <- unique(currenttracktime)

  }else{

    # get an index assigning each value of currenttrack to a time interval according to resolution and extract the time information from currenttrack
    indexcurrenttracktime <- assignTimeInterval(currenttrack = currenttrack, resolution = resolution)

    indexaggregatedtrackvalues <- lapply(unique(indexcurrenttracktime), function(x){
      seq_along(currenttracktime)[indexcurrenttracktime == x]
    })
    names(indexaggregatedtrackvalues) <- unique(currenttracktime)

  }

  # define an index assigning raster layers to elements of indexrasterlayertrackvalues
  indexrasterlayertrackvalues <- which(timedate %in% names(indexaggregatedtrackvalues))

  # extract the values
  do.call(c, lapply(seq_along(indexaggregatedtrackvalues), function(x){

    # define an index for the current data values of currenttrack
    ifelse(resolution != "movingwindowtendays", index <- indexaggregatedtrackvalues[[x]], index <- do.call(c, indexaggregatedtrackvalues[x:(x+9)]))

    # collect row indices with the same position
    indexvalueslocations <- tapply(seq_len(nrow(currenttrack@data[index,])), paste0(currenttrack$lon[index], "_", currenttrack$lat[index]), function(x)x)

    # get the first index for each location
    indexcurrenttracksubset <- sapply(indexvalueslocations, function(x) index[x[1]])

    # extract the respective data values of currenttrack and convert it to a SpatialPointsDataFrame and project it
    currenttracksubset <- TrackToSpatialPointsDataFrame(Track(track = STIDF(sp = currenttrack@sp[indexcurrenttracksubset], time = as.POSIXct(currenttrack$time[indexcurrenttracksubset]), data = currenttrack@data[indexcurrenttracksubset,], endTime = currenttrack$time[indexcurrenttracksubset])), crs = attributes(currenttrack@sp@proj4string)[[1]])

    # extract the respective values
    extract(raster[[indexrasterlayertrackvalues[x]]], currenttracksubset)

  }))

} # not tested yet for fixed ten-day interval resolution, moving window ten-day interval resolution, monthly resolution

# function in order to assign each time value of a Track object to a time interval corresponding to the specified resolution
assignTimeInterval <- function(currenttrack, resolution){

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
         months = {

           # aggregate to monthly resolution
           currenttracktime <- strftime(currenttrack$time, "%Y-%m")

           # get indices for each month
           indexcurrenttracktime <- as.factor(currenttracktime)

         })

  # return indexcurrenttracktime
  return(indexcurrenttracktime)

}
