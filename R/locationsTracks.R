#' @importFrom trajectories TracksCollection
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel clusterExport makeCluster stopCluster
#' @importFrom foreach foreach
NULL

#' Identifies and classifies visits in GPS tracks.
#'
#' \code{locationsTracks} identifies clusters (locations) of
#' points in GPS tracks (for all \code{\link[trajectories:Track-class]{Track}}
#' objects (\code{currenttrack}) in a
#' \code{\link[trajectories:Track-class]{Tracks}} object) using
#' \code{\link{locationsTrack}}.
#'
#' The function can be used in order to assign to each data
#' value of the \code{\link[trajectories:Track-class]{Track}}
#' objects of the input \code{\link[trajectories:Track-class]{Tracks}}
#' object an id of the cluster it is assigned to (
#' \code{summary = FALSE}) or to summarise the information
#' for each visit of a location (\code{summary = TRUE}).
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{Tracks}} object.
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
#' @param tmaxintersticenotvalid A \code{data.frame} object that defines
#' time periods in which the argument \code{tmaxinterstice} will be ignored,
#' i.e. visits at the same location are merged irrespective of the duration
#' between these visits if there is no campsite visit at a different location
#' in-between. Each row indicates a time period in which this should be valid.
#' \code{tmaxintersticenotvalid} must contain two columns: \code{start} represents
#' the start time of the time interval and \code{end} represents the end time of
#' the time interval.
#' It is evaluated for each visit if its endtime (\code{trackvisits$endtime})
#' is within any of the time periods or the starttime (\code{trackvisits$starttime})
#' of the next visit at the same location.
#' @param summary Logical value indicating if the information on the
#' locations and visits should be summarised (\code{summary = TRUE})
#' or not (\code{summary = FALSE}). See the details section for further
#' information.
#' @param night An integer vector with two elements:
#' \enumerate{
#'   \item The first element specifies the start hour of the night, e.g. \code{0}
#'   for 0 o'clock.
#'   \item The first element specifies the start hour of the night, e.g. \code{4}
#'   for 4 o'clock.
#' }
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel:clusterApply]{clusterCall}}.
#' @return
#' \describe{
#'   \item{If (\code{summary = FALSE})}{A
#'   \code{\link[trajectories:Track-class]{Tracks}} object with
#'   \code{\link[trajectories:Track-class]{Track}} objects that are identical
#'   to the input \code{\link[trajectories:Track-class]{Track}} objects, but
#'   have four additional columns in their \code{data} slot:
#'   \describe{
#'     \item{\code{location}}{An integer value for each identified
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
#'   \code{\link[trajectories:Track-class]{Track}} objects with the following variables:
#'   \describe{
#'     \item{location}{An integer value for each identified
#'     spatial point cluster (location) increasing with the time starting
#'     from 1.}
#'     \item{lon}{The longitude of the respective location (as mean value of
#'     the coordinates of the data values assigned to the visit).}
#'     \item{lat}{The latitude of the respective location (as mean value of
#'     the coordinates of the data values assigned to the visit).}
#'     \item{alt}{The altitude of the respective location (as mean value of
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
#'     \item{speed}{The speed of the respective location (as mean value of
#'     the speed values of the data values assigned to the visit).}
#'   }
#'   }
#' }
#' @seealso
#' \code{\link{locationsTrack}}.
#' @examples #
#' @export
locationsTracks <- function(currenttracks,
                            radius = 800,
                            tmin = 345600,
                            tmaxinterstices = 345600,
                            tmaxintersticenotvalid = data.frame(start = as.POSIXct("2016-01-01 00:00:00"), end = as.POSIXct("2016-05-01 00:00:00")),
                            summary = TRUE,
                            night = c(16, 20),
                            cores = 1,
                            clcall = NULL){

  # avoid no visible bindings message
  track_i <- NULL

  # set up cluster
  cl <- parallel::makeCluster(cores, outfile="", type = "PSOCK")
  doParallel::registerDoParallel(cl)
  if(is.null(clcall) == F){
    parallel::clusterCall(cl, clcall)
  }
  parallel::clusterExport(cl = cl, varlist = list("currenttracks", "tmin",
                                        "tmaxinterstices",
                                        "identifyBlocksVariable",
                                        "extractClustersBuffer", "redefineIndices",
                                        "tmaxintersticenotvalid", "trackvisits",
                                        "trackvisitsGetGroups", "trackvisitsMergeGroups",
                                        "trackvisitsFromTrack",
                                        "locationsTrack",
                                        "classifyNightTrack", "night"), envir=environment())

  # extract the names of currenttracks@tracks
  currenttracksnames <- names(currenttracks@tracksCollection)

  # apply locationsTrack to each Track object
  currenttracks <- foreach::foreach(track_i = seq_len(length(currenttracks@tracksCollection)), .packages = c("dbscan", "trajectories", "sp", "spacetime"))%dopar%{

    locationsTrack(currenttrack = currenttracks@tracksCollection[[track_i]]@tracks[[1]],
                   radius = radius,
                   tmin = tmin,
                   tmaxinterstices = tmaxinterstices,
                   tmaxintersticenotvalid = tmaxintersticenotvalid,
                   summary = summary,
                   night = night)

  }

  # get not null elements
  notnull <- which(!sapply(currenttracks, function(x) is.null(x)))

  # convert currenttracks to a Tracks object if summary == FALSE
  if(!summary){
    currenttracks <- trajectories::TracksCollection(lapply(currenttracks[notnull], function(x) Tracks(list(x))))
    names(currenttracks@tracksCollection) <- currenttracksnames[notnull]
  }else{
    currenttracks <- currenttracks[notnull]
    names(currenttracks) <- currenttracksnames[notnull]
  }

  parallel::stopCluster(cl)

  # return the result
  return(currenttracks)

}
