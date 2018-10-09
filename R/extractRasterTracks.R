#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import raster
#' @import doParallel
NULL

#' Extracts corresponding raster values for GPS tracks.
#'
#' \code{extractRasterTracks} extracts values from raster based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) that correspond the the respective
#' position and time of the \code{\link[trajectories]{Track}} objects of a
#' \code{\link[trajectories]{Tracks}} object.
#'
#' The \code{Track} object have to have data values for the same time
#' intervals and have to be of the same length. Time values have to be
#' stored in a column \code{time} in the \code{data} slot for each
#' \code{\link[trajectories]{Track}} object.
#'
#' @param currenttracks A \code{\link[trajectories]{Tracks}} object with
#' \code{\link[trajectories]{Track}} objects (\code{currenttrack}) as
#' described in the details section.
#' @param raster raster based time series (\code{RasterBrick} or
#' \code{RasterStack} object, see: \code{\link[raster]{Raster-class}}).
#' @param timedate A \code{POSIXct} vector with values for each layer of
#' \code{raster}.
#' @param resolution A character value indicating the temporal resolution
#' of \code{raster} and \code{timedate}, respectively. One of \code{"days"},
#' \code{"fixedtendays"}, \code{"movingwindowtendays"} and \code{"months"}.
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel]{clusterCall}}.
#' @return A vector with a value for each data value of \code{currenttrack}.
#' If a value of \code{raster} cannot be assigned to a respective data value,
#' \code{NA} is returned for the respective data value.
#' @seealso \code{\link{assignFixedTenDayInterval}},
#' \code{\link{extractRasterTracks}}.
#' @examples #
#' @export
extractRasterTracks <- function(currenttracks, raster, timedate, resolution, cores = 1, clcall = NULL){

  # extract the time information for currenttracks
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

  # get a list assigning the data values of eachTrack object to a time interval according to resolution
  if(resolution != "movingwindowtendays"){

    # get an index assigning each value of currenttrack to a time interval according to resolution and extract the time information from currenttrack
    indexcurrenttracktime <- assignTimeInterval(currenttrack = currenttracks@tracks[[1]], resolution = resolution)

    indexaggregatedtrackvalues <- lapply(unique(indexcurrenttracktime), function(x){
      seq_along(currenttracktime)[indexcurrenttracktime == x]
    })
    names(indexaggregatedtrackvalues) <- unique(currenttracktime)

  }else{

    # get an index assigning each value of currenttrack to a time interval according to resolution and extract the time information from currenttrack
    indexcurrenttracktime <- assignTimeInterval(currenttrack = currenttracks@tracks[[1]], resolution = "days")

    indexaggregatedtrackvalues <- lapply(unique(indexcurrenttracktime), function(x){
      seq_along(currenttracktime)[indexcurrenttracktime == x]
    })
    names(indexaggregatedtrackvalues) <- unique(currenttracktime)
  }

  # define an index assigning raster layers to elements of indexrasterlayertrackvalues
  indexrasterlayertrackvalues <- which(timedate %in% names(indexaggregatedtrackvalues))

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(is.null(clcall) == FALSE){
    clusterCall(cl, clcall)
  }
  clusterCall(cl, function(){library("spacetime")})
  clusterCall(cl, function(){library("trajectories")})
  clusterCall(cl, function(){library("raster")})
  clusterCall(cl, function(){library("sp")})
  clusterExport(cl = cl, varlist = list("currenttracks", "resolution",
                                        "indexaggregatedtrackvalues",
                                        "TrackToSpatialPointsDataFrame",
                                        "raster", "indexrasterlayertrackvalues"), envir=environment())

  # extract the values
  extractedvalues <- parLapply(cl, seq_along(indexaggregatedtrackvalues), function(x){

    print(x)

    # define an index for the current data values of currenttrack
    index <- indexaggregatedtrackvalues[[x]]

    # extract the respective data values of currenttrack and convert it to a SpatialPointsDataFrame and project it
    currenttracksubset <- do.call(rbind, lapply(seq_along(currenttracks@tracks), function(y){

      TrackToSpatialPointsDataFrame(Track(track = STIDF(sp = currenttracks@tracks[[y]]@sp[index], time = as.POSIXct(currenttracks@tracks[[y]]$time[index]), data = currenttracks@tracks[[y]]@data[index,], endTime = currenttracks@tracks[[y]]$time[index])))

    }))

    # extract the respective values
    extract(raster[[indexrasterlayertrackvalues[x]]], currenttracksubset)

  })

  # stop cluster
  stopCluster(cl)

  # reformat the output into a data.frame with a column for each input Track object
  indexcurrenttracks <- lapply(seq_along(indexaggregatedtrackvalues), function(x){

    # define an index for the current data values of currenttrack
    index <- indexaggregatedtrackvalues[[x]]

    do.call(c, lapply(seq_along(currenttracks@tracks), function(y){

      rep(y, length(index))

    }))

  })

  extractedvalues <- do.call(cbind, lapply(seq_along(currenttracks@tracks), function(x){

    do.call("c", lapply(seq_along(extractedvalues), function(y){

      extractedvalues[[y]][which(indexcurrenttracks[[y]] == x)]

    }))

  }))
  extractedvalues <- as.data.frame(extractedvalues, stringsAsFactors = FALSE)
  names(extractedvalues) <- row.names(currenttracks@tracksData)

  # return extractedvalues
  return(extractedvalues)

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
