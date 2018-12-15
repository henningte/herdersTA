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
#' position and time of the \code{\link[trajectories:Track-class]{Track}} objects of a
#' \code{\link[trajectories:Track-class]{Tracks}} object.
#'
#' The \code{Track} object have to have data values for the same time
#' intervals and have to be of the same length. Time values have to be
#' stored in a column \code{time} in the \code{data} slot for each
#' \code{\link[trajectories:Track-class]{Track}} object.
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{Tracks}} object with
#' \code{\link[trajectories:Track-class]{Track}} objects (\code{currenttrack}) as
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
extractRasterTracks <- function(currenttracks, raster, timedate, resolution, aggregation, cores = 1, clcall = NULL){

  # extract a sample Track object (representative since all Track objects are assumed to have the same time intervals)
  currenttrack <- currenttracks@tracks[[1]]

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
    currenttracks <- Tracks(lapply(seq_along(currenttracks@tracks), function(x){

      aggregatedvisits <- extractCoordinatesVisitsTrack(currenttrack = currenttracks@tracks[[x]],
                                                        aggregated = FALSE)

      aggregatedcurrenttrack <- currenttracks@tracks[[x]]
      aggregatedcurrenttrack@data$lon <- aggregatedvisits$lon
      aggregatedcurrenttrack@data$lat <- aggregatedvisits$lat
      aggregatedcurrenttrack@data$HEIGHT <- aggregatedvisits$alt
      aggregatedcurrenttrack@sp@coords <- cbind(aggregatedvisits$lon, aggregatedvisits$lat)
      colnames(aggregatedcurrenttrack@sp@coords) <- colnames(currenttrack@sp@coords)

      return(aggregatedcurrenttrack)

    }))

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

    # define an index for the current data values of currenttrack
    ifelse(resolution != "movingwindowtendays", index <- indexaggregatedtrackvalues[[x]], index <- do.call(c, indexaggregatedtrackvalues[x:(x+9)]))

    # extract the respective data values of currenttrack and convert it to a SpatialPointsDataFrame and project it
    currenttracksubset <- do.call(rbind, lapply(seq_along(currenttracks@tracks), function(y){

      # collect row indices with the same position
      index2 <- which(currenttracks@tracks[[y]]@data$gap[index] == FALSE)
      if(length(index2) > 0){

        indexvalueslocations <- tapply(seq_len(nrow(currenttracks@tracks[[y]]@data[index,]))[index2], paste0(currenttracks@tracks[[y]]$lon[index][index2], "_", currenttracks@tracks[[y]]$lat[index][index2]), function(z)z)

        # get the first index for each location
        indexcurrenttracksubset <- sapply(indexvalueslocations, function(z) index[z[1]])

        # extract the respective data values of currenttrack and convert it to a SpatialPointsDataFrame and project it
        currenttracksubset <- currenttracks@tracks[[y]]@sp[indexcurrenttracksubset]

        trackid <- data.frame(trackid = rep(y, nrow(currenttracksubset@coords)), gap = rep(FALSE, nrow(currenttracksubset@coords)))

        coordinates(trackid) <- coordinates(currenttracksubset)

        trackid

      }else{

        currenttracksubset <- currenttracks@tracks[[y]]@sp[1]

        trackid <- data.frame(trackid = rep(y, nrow(currenttracksubset@coords)), gap = rep(TRUE, nrow(currenttracksubset@coords)))

        dummycoordinates <- coordinates(currenttracksubset)
        dummycoordinates[1:2] <- c(0, 0)

        coordinates(trackid) <- dummycoordinates

        trackid

      }


    }))

    # extract the respective values
    extractedvalues <- extract(raster[[indexrasterlayertrackvalues[x]]], currenttracksubset)

    # get information on gaps
    gaps <- tapply(currenttracksubset$gap, currenttracksubset$trackid, function(x)x[1])

    # average the extracted values
    averagedextractedvalues <- tapply(extractedvalues, currenttracksubset$trackid, function(y){if(length(y) == 1 && is.na(y)){NA}else{na.omit(mean(y))}})

    averagedextractedvalues[which(gaps == TRUE)] <- NA

    averagedextractedvalues

  })

  # stop cluster
  stopCluster(cl)

  extractedvalues <- do.call(rbind, extractedvalues)
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

###


