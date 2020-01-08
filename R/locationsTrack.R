#' @importFrom plyr join
NULL

#' Identifies and classifies visits in GPS tracks.
#'
#' \code{locationsTrack} identifies clusters (locations) of
#' points in GPS tracks (\code{\link[trajectories:Track-class]{Track}}
#' object) based on their spatial proximity using
#' \code{\link{extractClustersBuffer}} and identifies individual
#' visits of the same cluster (location) along the track.
#' Additionally, visits are classified as long-term visits
#' (campsites) or short-term visits. During the identification of
#' clusters, data values within a user defined daily time interval
#' are considered. The remaining values are set to the next
#' location identified for the adjacent time intervals. This
#' procedure can be used in order to identify campsites by
#' assuming that a household has a campsite where it stayed
#' over night. If values outside the defined time interval
#' have a too far distance, they are classified as short-term
#' visit.
#'
#' The function can be used in order to assign to each data
#' value of the input \code{\link[trajectories:Track-class]{Track}}
#' object an id of the cluster it is assigned to (
#' \code{summary = FALSE}) or to summarise the information
#' for each visit of a location (\code{summary = TRUE}).
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object.
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
#' @param night An integer vector with two elements: The first element specifies
#' the start hour of the night, e.g. \code{0} for 0 o'clock. The second element
#' specifies the start hour of the night, e.g. \code{4} for 4 o'clock.
#' @param tmaxintersticenotvalid A \code{data.frame} object that defines
#' time periods in which the argument \code{tmaxinterstice} will be ignored,
#' i.e. visits at the same location are merged irrespective of the duration
#' between these visits if there is no campsite visit at a different location
#' in-between. Each row indicates a time period in which this should be valid.
#' \code{tmaxintersticenotvalid} must contain two columns:
#' \describe{
#'   \item{\code{start}}{Represents the start time of the time interval.}
#'   \item{\code{end}}{Represents the end time of the time interval.}
#' }
#' It is evaluated for each visit if its endtime (\code{trackvisits$endtime})
#' is within any of the time periods or the starttime (\code{trackvisits$starttime})
#' of the next visit at the same location.
#' @param summary Logical value indicating if the information on the
#' locations and visits should be summarised (\code{summary = TRUE})
#' or not (\code{summary = FALSE}). See the details section for further
#' information.
#' @return
#' \describe{
#'   \item{If (\code{summary = FALSE})}{A
#'   \code{\link[trajectories:Track-class]{Track}} object that is identical
#'   to the input \code{\link[trajectories:Track-class]{Track}} object, but
#'   has five additional columns in the \code{data} slot:
#'   \describe{
#'     \item{\code{location}}{A numeric integer value for each identified
#'     spatial point cluster (location) increasing with the time starting.}
#'     \item{\code{visit}}{A numeric integer value for each identified
#'     visit increasing with the time starting.}
#'     \item{\code{campsite}}{A logical value indicating if a visits of a
#'     location is classified as long-term visit (campsite) (\code{TRUE})
#'     or as short-term visit (\code{FALSE}).}
#'     \item{\code{norepeatedcampsitevisits}}{An integer vector indicating the number of
#'     campsites at a specific location the data point is assigned to until the
#'     current visit.}
#'     \item{\code{start}}{A logical vector indicating if a value represents the
#'     first value of a visit (\code{start = TRUE}) or not (\code{start = FALSE}).}
#'     \item{\code{end}}{A logical vector indicating if a value represents the
#'     last value of a visit (\code{end = TRUE}) or not (\code{end = FALSE}).}
#'   }
#'   Gaps, as indicated by the column \code{gap}, have \code{NA} values for
#'   \code{location}, \code{campsite}, \code{norepeatedcampsitevisits} and \code{FALSE}
#'   values for \code{start} and \code{end}.
#'   }
#'   \item{If (\code{summary = TRUE})}{An object of class \code{\link{trackvisits}}
#'   summarising the locations and visits for \code{currenttrack}.}
#' }
#' @seealso
#' \code{\link{locationsTracks}}.
#' @examples #
#' @export
locationsTrack <- function(currenttrack,
                           radius = 800,
                           tmin = 345600,
                           tmaxinterstices = 345600,
                           night = c(16, 20),
                           tmaxintersticenotvalid = data.frame(start = as.POSIXct("2016-01-01 00:00:00"), end = as.POSIXct("2016-05-01 00:00:00")),
                           summary = TRUE){

  # extract the time interval of adjacent values in currenttrack
  timeinterval <- as.numeric(difftime(time1 = as.POSIXct(currenttrack@time)[2], time2 = as.POSIXct(currenttrack@time)[1], units = "secs"))

  # append the information on whether a data value was ecorded during night or day
  currenttrack <- classifyNightTrack(currenttrack, night)

  # cluster the data points
  currenttrack$location <- extractClustersBuffer(currenttrack = currenttrack, radius = radius)

  # return NULL if a data set contains only one location (no Track object can be constructed from one point)
  if(length(unique(currenttrack$location)) == 1){
    warning("Only one location identified!")
    return(NULL)
  }

  # redefine indices of locations (according to arrival time)
  currenttrack$location <- redefineIndices(x = currenttrack$location, notchange = 0)

  # adjust variable formats and names
  names(currenttrack@data)[names(currenttrack@data) == "HEIGHT"] <- "altitude"
  currenttrack$altitude <- as.numeric(currenttrack$altitude)

  # extract visits
  currenttrackvisits <- trackvisitsFromTrack(currenttrack = currenttrack, tmin = tmin)

  # group and merge visits at adjacent days (day gaps due to the consideration of only nightvalues)
  currenttrackvisits <- trackvisitsGetGroups(trackvisits = currenttrackvisits, tmin = tmin, timeinterval = timeinterval, tmaxinterstice = 24*60*60, tmaxintersticenotvalid = NULL)
  currenttrackvisits <- trackvisitsMergeGroups(currenttrackvisits = currenttrackvisits, tmin = tmin, timeinterval = timeinterval, keepgroup = FALSE)

  # group and merge visits with gaps < tmaxinterstices
  currenttrackvisits <- trackvisitsGetGroups(trackvisits = currenttrackvisits, tmin = tmin, timeinterval = timeinterval, tmaxinterstice = tmaxinterstices, tmaxintersticenotvalid = NULL)
  currenttrackvisits <- trackvisitsMergeGroups(currenttrackvisits = currenttrackvisits, tmin = tmin, timeinterval = timeinterval, keepgroup = FALSE)

  # group and merge visits in special time intervals
  currenttrackvisits <- trackvisitsGetGroups(trackvisits = currenttrackvisits, tmin = tmin, timeinterval = timeinterval, tmaxinterstice = tmaxinterstices, tmaxintersticenotvalid = tmaxintersticenotvalid)
  currenttrackvisits <- trackvisitsMergeGroups(currenttrackvisits = currenttrackvisits, tmin = tmin, timeinterval = timeinterval, keepgroup = TRUE)

  # return the results
  if(!summary){

    # get data to insert into currenttrack
    addtocurrenttrackdata <- currenttrackvisits[,c("group", "campsite", "norepeatedcampsitevisits")]

    # redefine group and location in currenttrack (update to merged visits), initialise start and end values of visits
    currenttrack$group <- 0
    currenttrack$location <- 0
    currenttrack$start <- FALSE
    currenttrack$end <- FALSE
    sapply(seq_len(nrow(currenttrackvisits)), function(x){
      currenttrack$group[currenttrackvisits$start[x]:currenttrackvisits$end[x]] <<- currenttrackvisits$group[x]
      currenttrack$location[currenttrackvisits$start[x]:currenttrackvisits$end[x]] <<- currenttrackvisits$location[x]
      currenttrack$filled[currenttrackvisits$start[x]:currenttrackvisits$end[x]] <<- ifelse(currenttrack$gap[currenttrackvisits$start[x]:currenttrackvisits$end[x]], TRUE, FALSE)
      currenttrack$gap[currenttrackvisits$start[x]:currenttrackvisits$end[x]] <<- FALSE
      currenttrack$start[currenttrackvisits$start[x]] <<- TRUE
      currenttrack$end[currenttrackvisits$end[x]] <<- TRUE
    })

    # merge currenttrack@data and addtocurrenttrack
    currenttrack@data <- plyr::join(x = currenttrack@data, y = addtocurrenttrackdata, by = "group", type = "left")

    # set all values for currenttrack@data$gap to TRUE for currenttrack@data$location == 0. This has to be done here because locationsTrack redefines gaps because data values are not assigned to any location (i.e. get location = 0) if there are too few points for an own cluster.
    currenttrack$gap[currenttrack$location == 0] <- TRUE
    currenttrack$filled[currenttrack$location == 0] <- FALSE

    # return currenttrack
    return(currenttrack)

  }else{

    # return currenttrackvisits
    return(currenttrackvisits)

  }

}
