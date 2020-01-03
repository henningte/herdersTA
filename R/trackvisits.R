#' @importFrom lubridate is.POSIXct
NULL

#' Visits from a Track with Locations
#'
#' \code{trackvisits} is the constructor function of objects of class
#' \code{trackvisits}. Object of class \code{trackvisits} are
#' \code{data.frame} objects that contain a row for each visit at a
#' specific location and several variables for this visit:
#' \describe{
#'   \item{\code{location}}{A numeric value representing the id of the
#'   location.}
#'   \item{\code{group}}{A numeric value representing the id of the group
#'   the visit is assigned to.}
#'   \item{\code{start}}{A numeric value representing a row index giving
#'   the start of the visit.}
#'   \item{\code{end}}{A numeric value representing a row index giving
#'   the end of the visit.}
#'   \item{\code{nextvisitduration}}{A numeric value representing the duration
#'   between the end of the current visit and the start of the following
#'   visit in row numbers.}
#'   \item{\code{nextvisitsamelocationduration}}{A numeric value representing
#'   the duration between the end of the current visit and the start of the next
#'   following visit at the same location in row numbers.}
#'   \item{\code{nextvisitsamelocation}}{A logical value indicating if the
#'   following visit is at the same location (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{\code{campsite}}{A logical value indicating of the current visit
#'   is classified as campsite (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{\code{campsiteinbetween}}{A logical value indicating if there is a
#'   campsite visit at a different location between the end of the current visit
#'   and the start of the next following visit at the same location.}
#'   \item{\code{mergewithnext}}{A logical value indicating if the current visit
#'   should be merged with the next visit (\code{TRUE}) or not (\code{FALSE}).}
#' }
#' Object of class \code{trackvisits} can be used in order to manipulate tracks
#' based on visits and locations.
#'
#' @param location A numeric value representing the id of the locations.
#' @param group A numeric value representing the id of the group the
#' visits are assigned to.
#' @param start A numeric value representing a row index giving the start
#' of the visits.
#' @param end A numeric value representing a row index giving the end
#' of the visits.
#' @param starttime A \code{POSIXct} value representing the time
#' of the start of the visit.
#' @param endtime A \code{POSIXct} value representing the time
#' of the end of the visit.
#' @param longitude A numeric value representing the longitude of the
#' location of the visit.
#' @param latitude A numeric value representing the latitude of the
#' location of the visit.
#' @param altitude A numeric value representing the altitude of the
#' location of the visit.
#' @param currentvisitduration A numeric value representing the duration
#' between the end of the current visit and the start of the current
#' visit in row numbers.
#' @param nextvisitsduration A numeric value representing the duration
#' between the end of the current visit and the start of the following
#' visit in row numbers. The last value must be \code{NA}.
#' @param nextvisitsamelocationindex A numeric value representing the index of
#' the visit that is at the same location as the current visit. The last value
#' must be \code{NA}.
#' @param nextvisitssamelocationduration A numeric value representing the duration
#' between the end of the current visit and the start of the next
#' following visit at the same location in row numbers. The last value
#' must be \code{NA}.
#' @param nextvisitsamelocation A logical value indicating of the current visit
#' is classified as campsite (\code{TRUE}) or not (\code{FALSE}).
#' @param campsite A logical value indicating of the current visit
#' is classified as campsite (\code{TRUE}) or not (\code{FALSE}).
#' @param campsiteinbetween A logical value indicating if there is a
#' campsite visit at a different location between the end of the current visit
#' and the start of the next following visit at the same location.
#' @param mergewithnext A logical value indicating if the current visit
#' should be merged with the next visit (\code{TRUE}) or not (\code{FALSE}).
#' If set to \code{NULL}, \code{NA} will be inserted.
#' @param currenttrack A \code{data.frame} object with a variable
#' \code{location} that is an id for locations. Gap values must have
#' the value \code{0} for \code{location}.
#' @param tmin The minimum residence time at a specific location [s]
#' which is used to classify visits as campsites (if the residence
#' time at a specific location is larger than \code{tmin}) in
#' contrast to short-term visits of locations. The default is
#' \code{tmin = 345600}, i.e. 4 days.
#' @param tmaxinterstices The maximum time threshold between subsequent
#' visits at the same location [s]. If the duration between the two visits
#' is \eqn{<} \code{tmaxinterstices} and if there is no other visit
#' classified as campsite in-between, the two visits will be merged to one
#' visit.
#' @param timeinterval The number of seconds one data value (row) in
#' \code{currenttrack} covers.
#' @return An object of class \code{trackvisits}. Object of class \code{trackvisits} are
#' \code{data.frame} objects that contain a row for each visit at a
#' specific location and several variables:
#' \describe{
#'   \item{\code{location}}{A numeric value representing the id of the
#'   location.}
#'   \item{\code{group}}{A numeric value representing the id of the group
#'   the visit is assigned to.}
#'   \item{\code{start}}{A numeric value representing a row index giving
#'   the start of the visit.}
#'   \item{\code{end}}{A numeric value representing a row index giving
#'   the end of the visit.}
#'   \item{\code{starttime}}{A \code{POSIXct} value representing the time
#'   of the start of the visit.}
#'   \item{\code{endtime}}{A \code{POSIXct} value representing the time
#'   of the end of the visit.}
#'   \item{\code{longitude}}{A numeric value representing the longitude of the
#'   location of the visit.}
#'   \item{\code{latitude}}{A numeric value representing the latitude of the
#'   location of the visit.}
#'   \item{\code{altitude}}{A numeric value representing the altitude of the
#'   location of the visit.}
#'   \item{\code{currentvisitduration}}{A numeric value representing the duration
#'   between the end of the current visit and the start of the current
#'   visit in row numbers.}
#'   \item{\code{nextvisitduration}}{A numeric value representing the duration
#'   between the end of the current visit and the start of the following
#'   visit in row numbers.}
#'   \item{\code{nextvisitsamelocationindex}}{A numeric value representing the index of
#'   the visit that is at the same location as the current visit.}
#'   \item{\code{nextvisitsamelocationduration}}{A numeric value representing
#'   the duration between the end of the current visit and the start of the next
#'   following visit at the same location in row numbers.}
#'   \item{\code{nextvisitsamelocation}}{A logical value indicating if the
#'   following visit is at the same location (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{\code{campsite}}{A logical value indicating of the current visit
#'   is classified as campsite (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{\code{campsiteinbetween}}{A logical value indicating if there is a
#'   campsite visit at a different location between the end of the current visit
#'   and the start of the next following visit at the same location.}
#'   \item{\code{mergewithnext}}{A logical value indicating if the current visit
#'   should be merged with the next visit (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{\code{norepeatedcampsitevisits}}{A numeric value representing the number
#'   of repeated campsite visits at the same location until this visit.}
#' }
#'
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{redefineIndices}},
#' \code{\link{fillGapTrack}}, \code{\link{fillGapTracks}},
#' \code{\link{extractClustersBuffer}}.
#' @examples #
#' @export
trackvisits <- function(location,
                        group = NA,
                        start,
                        end,
                        starttime,
                        endtime,
                        longitude,
                        latitude,
                        altitude,
                        currentvisitduration,
                        nextvisitduration = NA,
                        nextvisitsamelocationindex = NA,
                        nextvisitsamelocationduration = NA,
                        nextvisitsamelocation = FALSE,
                        campsite = FALSE,
                        campsiteinbetween = FALSE,
                        mergewithnext = FALSE,
                        norepeatedcampsitevisits = NA) {

  # checks
  if(!(is.numeric(location) || !(0 %in% location) || all(location %% 1 == 0))){
    stop("location must be a numeric vector with integers and must not contain 0\n")
  }
  if(!(is.numeric(group) || all(group %% 1 == 0) || is.na(group))){
    stop("group must be a numeric vector with integers or NA\n")
  }
  if(!(is.numeric(start) || all(start %% 1 == 0))){
    stop("start must be a numeric vector with integers\n")
  }
  if(!(is.numeric(end) || all(end %% 1 == 0))){
    stop("end must be a numeric vector with integers\n")
  }
  if(!(is.POSIXct(starttime))){
    stop("starttime must be a POSIXct vector\n")
  }
  if(!(is.POSIXct(endtime))){
    stop("endtime must be a POSIXct vector\n")
  }
  if(!(is.numeric(longitude))){
    stop("longitude must be a numeric vector\n")
  }
  if(!(is.numeric(latitude))){
    stop("latitude must be a numeric vector\n")
  }
  if(!(is.numeric(altitude))){
    stop("altitude must be a numeric vector\n")
  }
  if(!(is.numeric(currentvisitduration) || all(currentvisitduration %% 1 == 0))){
    stop("currentvisitduration must be a numeric vector with integers\n")
  }
  if(!(is.numeric(nextvisitduration) || all(nextvisitduration %% 1 == 0) || nextvisitduration[length(nextvisitduration)] == 0 || is.na(nextvisitduration))){
    stop("nextvisitduration must be a numeric vector with integers and the last value must be 0\n")
  }
  if(!(is.na(nextvisitsamelocationindex) || is.numeric(nextvisitsamelocationindex) || all(nextvisitsamelocationindex %% 1 == 0) || is.na(nextvisitsamelocationindex[length(nextvisitsamelocationindex)]))){
    stop("nextvisitsamelocationindex must be a numeric vector with integers and the last value must be NA\n")
  }
  if(!(is.na(nextvisitsamelocationduration) || is.numeric(nextvisitsamelocationduration) || all(nextvisitsamelocationduration %% 1 == 0) || is.na(nextvisitsamelocationduration[length(nextvisitsamelocationduration)]))){
    stop("nextvisitsamelocationduration must be a numeric vector with integers and the last value must be NA\n")
  }
  if(!(is.logical(nextvisitsamelocation) || nextvisitsamelocation[length(nextvisitsamelocation)] == FALSE)){
    stop("nextvisitsamelocation must be a logical vector and the last value must be FALSE\n")
  }
  if(!(is.logical(campsite) || (length(campsite) ==1 && campsite == FALSE))){
    stop("campsite must be a logical vector\n")
  }
  if(!(is.logical(campsiteinbetween) || campsiteinbetween[length(campsiteinbetween)] == FALSE)){
    stop("campsiteinbetween must be a logical vector and the last value must be FALSE\n")
  }
  if(!(is.logical(mergewithnext) || campsiteinbetween[length(mergewithnext)] == FALSE)){
    stop("mergewithnext must be a logical vector and the last value must be FALSE\n")
  }
  if(!(is.numeric(norepeatedcampsitevisits))){
    stop("norepeatedcampsitevisits must be a numeric vector\n")
  }

  # construct the trackvisits object
  structure(
    data.frame(location = location,
               group = group,
               start = start,
               end = end,
               starttime = starttime,
               endtime = endtime,
               longitude = longitude,
               latitude = latitude,
               altitude = altitude,
               currentvisitduration = currentvisitduration,
               nextvisitduration = nextvisitduration,
               nextvisitsamelocationindex = nextvisitsamelocationindex,
               nextvisitsamelocationduration = nextvisitsamelocationduration,
               nextvisitsamelocation = nextvisitsamelocation,
               campsite = campsite,
               campsiteinbetween = campsiteinbetween,
               mergewithnext = mergewithnext,
               norepeatedcampsitevisits = norepeatedcampsitevisits,
               stringsAsFactors = FALSE),
    class = c("trackvisits", "data.frame"))

}
