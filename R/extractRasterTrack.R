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
           indexcurrenttracktime <- assigntendayinterval(as.POSIXct(currenttracktime))

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

}

# function in order to classify on a ten-day interval resolution
assigntendayinterval <- function(timedate){

  # get the years covered
  years <- unique(strftime(timedate, format = "%Y"))

  # get the number of days in each year
  days <- list()
  for(year_i in seq_along(years)){
    days[[year_i]] <- seq(as.POSIXct(paste0(years[year_i], "-01-01"), tz = attr(timedate, "tzone")), as.POSIXct(paste0(years[year_i], "-12-31"), tz = attr(timedate, "tzone")), "day")
  }

  # create a list with indices for days
  tendayintervals <- lapply(seq_along(days), function(y){

    # extract months
    months <- strftime(days[[y]], format = "%y-%m")

    # define an additional offset value if data for several years exist there
    offset <- (y - 1) * 36

    # get a vector with ten day-intervals
    tdi <- do.call(c, as.vector(sapply(seq_along(table(months)), function(x){

      # get indices of ten day-intervals
      if(which(names(table(months)) == names(table(months)[x])) == 1){
        indices <- 1:3
      }else{
        indices <- seq((which(names(table(months)) == names(table(months)[x]))-1) * 3 + 1, (which(names(table(months)) == names(table(months)[x]))-1) * 3 + 3)
      }

      c(rep(indices[1], 10), rep(indices[2], 10), rep(indices[3], table(months)[x] - 20))

    }))) + offset

  })

  # assign each day to a ten day-interval
  tendayintervals <-
    do.call(c, sapply(seq_along(days), function(x){
      names(tendayintervals[[x]]) <- days[[x]]
      return(tendayintervals[[x]])
    }))

  # get unique ten day-intervals for each data value in track
  timeinterval <- rep(0, length(timedate))
  days <- strftime(timedate, format = "%Y-%m-%d")
  iter <- 1
  iter2 <- 0
  for(day_i in unique(days)){
    timeinterval[which(days == day_i)] <- tendayintervals[which(names(tendayintervals) == day_i)] - iter2*36

    # reset iter after one year
    if((tendayintervals[which(names(tendayintervals) == day_i)] %% 36 == 0) && strftime(day_i, format = "%m-%d") == "12-31"){
      iter <- 1
      iter2 <- iter2 + 1
    }
  }

  return(timeinterval)

} # not tested yet for fixed ten-day interval resolution, moving window ten-day interval resolution, monthly resolution
