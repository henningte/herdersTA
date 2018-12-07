#' @importFrom Rdpack reprompt
#' @import spacetime
#' @import lubridate
#' @import trajectories
#' @import rgdal
#' @import sp
#' @import rgeos
#' @import doParallel
NULL

#' Identifies and classifies visits in GPS tracks.
#'
#' \code{locationsTracks} identifies clusters (locations) of
#' points in GPS tracks (for all \code{\link[trajectories]{Track}}
#' objects (\code{currenttrack}) in a
#' \code{\link[trajectories]{Tracks}} object) using
#' \code{\linkn{locationsTrack}}.
#'
#' The function can be used in order to assign to each data
#' value of the \code{\link[trajectories]{Track}}
#' objects of the input \code{\link[trajectories]{Tracks}}
#' object an id of the cluster it is assigned to (
#' \code{summary = FALSE}) or to summarise the information
#' for each visit of a location (\code{summary = TRUE}).
#'
#' @param currenttracks A \code{\link[trajectories]{Tracks}} object.
#' @param radius A numerical value representing the radius of the
#' buffers computed around each point [m] which are used for
#' clustering values to locations. Default is \code{radius = 800} [m].
#' @param tmin The minimum residence time at a specific location [s]
#' which is used to classify visits as campsites (if the residence
#' time at a specific location is larger than \code{tmin}) in
#' contrast to short-term visits of locations. The default is
#' \code{tmin = 345600}, i.e. 4 days.
#' @param tmaxinterstices The maximum time between subsequent visits
#' at the same location in case of which the duration of these visits
#' will be added in order to classify both visits together as long-term
#' visit (campsite) or short-term visit, based on \code{tmin}.
#' @param timeinterval A numerical value reperesenting the duration
#' of a time interval represented by one data value of
#' \code{currenttrack} [s].
#' @param summary Logical value indicating if the information on the
#' locations and visits should be summarised (\code{summary = TRUE})
#' or not (\code{summary = FALSE}). See the details section for further
#' information.
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel]{clusterCall}}.
#' @return
#' \describe{
#'   \item{If (\code{summary = FALSE})}{A
#'   \code{\link[trajectories]{Tracks}} object with
#'   \code{\link[trajectories]{Track}} objects that are identical
#'   to the input \code{\link[trajectories]{Track}} objects, but
#'   have four additional columns in their \code{data} slot:
#'   \describe{
#'     \item {\code{location}}{An integer value for each identified
#'     spatial point cluster (location) increasing with the time starting
#'     from 1.}
#'     \item{\code{campsite}}{A logical value indicating if a visits of a
#'     location is classified as long-term visit (campsite) (\code{TRUE})
#'     or as short-term visit (\code{FALSE}).}
#'     \item{\code{visitsloc}}{An integer vector indicating the number of
#'     visits at a specific location the data point is assigned to (i.e.
#'     a counter for the visits at a specific location).}
#'     \item{\code{visitscampsite}}{An integer vector indicating the number
#'     of visits at a specific location the data point is assigned to that
#'     are classified as long-term visits (campsites) (i.e.
#'     a counter for the long-term visits at a specific location).}
#'   }
#'   Gaps, as indicated by the column \code{gap}, have \code{NA} values for
#'   all four variables.
#'   }
#'   \item{If (\code{summary = TRUE})}{A list of \code{data.frame}
#'   objects summarising the locations and visits of the input
#'   \code{\link[trajectories]{Track}} objects with the following variables:
#'   \describe{
#'     \item{location}{An integer value for each identified
#'     spatial point cluster (location) increasing with the time starting
#'     from 1.}
#'     \code{lon}{The longitude of the respective location (as mean value of
#'     the coordinates of the data values assigned to the visit).}
#'     \code{lat}{The latitude of the respective location (as mean value of
#'     the coordinates of the data values assigned to the visit).}
#'     \code{alt}{The altitude of the respective location (as mean value of
#'     the coordinates of the data values assigned to the visit).}
#'     \item{\code{visitsloc}}{An integer vector indicating the number of
#'     visits at a specific location the data point is assigned to (i.e.
#'     a counter for the visits at a specific location).}
#'     \item{campsite}{A logical value indicating if a visits of a
#'     location is classified as long-term visit (campsite) (\code{TRUE})
#'     or as short-term visit (\code{FALSE}).}
#'     \item{\code{visitscampsite}}{An integer vector indicating the number
#'     of visits at a specific location the data point is assigned to that
#'     are classified as long-term visits (campsites) (i.e.
#'     a counter for the long-term visits at a specific location).}
#'     \item{arrivaltime}{A \code{POSIXct} vector indicating the arrival time
#'     of the visit at the respective location.}
#'     \item{departuretime}{A \code{POSIXct} vector indicating the departure
#'     time of the visit from the respective location.}
#'     \item{residencetime}{A numerical vector indicating the residence time
#'     of each visit [s].}
#'     \code{speed}{The speed of the respective location (as mean value of
#'     the speed values of the data values assigned to the visit).}
#'   }
#'   }
#' }
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{redefineIndices}},
#' \code{\link{fillGapTrack}}, \code{\link{fillGapTracks}},
#' \code{\link{extractClustersBuffer}}.
#' @examples #
#' @export
locationsTracks <- function(currenttracks,
                            radius = 200,
                            tmin = 345600,
                            tmaxinterstices = 345600,
                            timeinterval = 30*60,
                            crs = "+proj=utm +zone=46 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                            summary = TRUE,
                            cores = 1,
                            clcall = NULL){

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(is.null(clcall) == F){
    clusterCall(cl, clcall)
  }
  clusterCall(cl, function(){library("spacetime")})
  clusterCall(cl, function(){library("trajectories")})
  clusterCall(cl, function(){library("raster")})
  clusterCall(cl, function(){library("dbscan")})
  clusterCall(cl, function(){library("sp")})
  clusterExport(cl = cl, varlist = list("currenttracks", "tmin",
                                        "tmaxinterstices", "timeinterval",
                                        "identifyBlocksVariable",
                                        "extractClustersBuffer", "redefineIndices",
                                        "countAllReapeatedVisits",
                                        "aggregateRepeatedVisits", "classifyVisits",
                                        "countAllReapeatedLongTermVisits",
                                        "locationsTrack", "crs",
                                        "TrackToSpatialPointsDataFrame",
                                        "classifyNightTrack"), envir=environment())

  # extract the names of currenttracks@tracks
  currenttracksnames <- names(currenttracks@tracks)

  # apply locationsTrack to each Track object
  currenttracks <- foreach(track_i = seq_len(length(currenttracks@tracks)), .multicombine = TRUE)%dopar%{

    locationsTrack(currenttrack = currenttracks@tracks[[track_i]],
                   radius = radius,
                   tmin = tmin,
                   tmaxinterstices = tmaxinterstices,
                   timeinterval = timeinterval,
                   crs = crs,
                   summary = summary)

  }

  # convert currenttracks to a Tracks object if summary == FALSE
  if(summary == FALSE){
    currenttracks <- Tracks(currenttracks)
    names(currenttracks@tracks) <- currenttracksnames
  }else{
    names(currenttracks) <- currenttracksnames
  }

  stopCluster(cl)

  # return the result
  return(currenttracks)

}
