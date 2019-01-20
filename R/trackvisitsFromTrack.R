#' @importFrom Rdpack reprompt
#' @importFrom data.table rbindlist
#' @importFrom lubridate is.POSIXct
NULL

#' Extracts Visits from Track Objects
#'
#' \code{trackvisitsFromTrack} extracts an object of class
#' \code{\link{trackvisits}} from an object of class
#' \code{\link[trajectories:Track-class]{Track}}.
#'
#' @param currenttrack A \code{data.frame} object with a variable \code{time}
#' that represents the time as \code{POSIXct} vector and
#' with a variable \code{location} that is an id for locations. Gap values
#' must have the value \code{0} for \code{location}. \code{currenttrack}
#' must contain data values for equally spaced time intervals, i.e. each value
#' must correspond to a time interval of the same length.
#' @param tmin The minimum residence time at a specific location [s]
#' which is used to classify visits as campsites (if the residence
#' time at a specific location is larger than \code{tmin}) in
#' contrast to short-term visits of locations. The default is
#' \code{tmin = 345600}, i.e. 4 days.
#' @param timeinterval The number of seconds one data value (row) in
#' \code{currenttrack} covers.
#' @return An object of class \code{\link{trackvisits}}.
#'
#' @seealso \code{\link{trackvisits}}.
#' @examples #
#' @export
trackvisitsFromTrack <- function(currenttrack, tmin = 345600, timeinterval = 30*60){

  # checks
  if(!inherits(currenttrack, "data.frame")){
    stop("currenttrack must be of class data.frame\n")
  }
  if(!("location" %in% colnames(currenttrack))){
    stop("currenttrack must contain a variable 'location'\n")
  }
  if(!("time" %in% colnames(currenttrack))){
    stop("currenttrack must contain a variable 'time'\n")
  }
  if(!(is.numeric(tmin) || tmin > 0)){
    stop("tmin must be a numeric value > 0\n")
  }
  if(!(is.numeric(timeinterval) || timeinterval > 0)){
    stop("timeinterval must be a numeric value > 0\n")
  }

  # extract visits for each location
  visits <- data.table::rbindlist(lapply(unique(currenttrack$location), function(x){
    if(x != 0){
      currentblock <- identifyBlocksVariable(currenttrack = currenttrack, variable = "location", value = x)
      currentblock$location <- rep(x, nrow(currentblock))
      currentblock
    }
  }))

  # sort visits according to visits$start
  visits <- visits[order(visits$start),]

  # extract the respective start and end times
  visits$starttime <- currenttrack$time[visits$start]
  visits$endtime <- currenttrack$time[visits$end]

  # extract the respective median coordinates
  visits$longitude <- sapply(seq_len(nrow(visits)), function(x){
    median(currenttrack$longitude[visits$start[x]:visits$end[x]])
  })
  visits$latitude <- sapply(seq_len(nrow(visits)), function(x){
    median(currenttrack$latitude[visits$start[x]:visits$end[x]])
  })
  visits$altitude <- sapply(seq_len(nrow(visits)), function(x){
    median(currenttrack$altitude[visits$start[x]:visits$end[x]])
  })

  # set the dummy class
  class(visits) <- c("trackvisits", "data.frame")

  # compute the duration to the next visit
  visits$nextvisitduration <- c(visits$start[-1] - visits$end[-nrow(visits)], NA)

  # check if the next visit is at the same location
  visits$nextvisitsamelocation <- c(ifelse(visits$location[-1] == visits$location[-nrow(visits)], TRUE, FALSE), FALSE)

  # get for each visit the row index of the next visit at the same location
  visits <- trackvisitsSetNextvisitsamelocationindex(currenttrackvisits = visits)

  # get for each visit the duration until the next visit at the same location
  visits$nextvisitsamelocationduration <- sapply(seq_len(nrow(visits)), function(x){

    # compute the duration to this visit
    if(length(visits$nextvisitsamelocationindex[x]) > 0){
      visits$start[visits$nextvisitsamelocationindex[x]] - visits$end[x]
    }else{
      NA
    }

  })

  # get for each visit the duration of the current visit
  visits$currentvisitduration <- visits$end - visits$start

  # check for each visit if it is a campsite visit or not
  visits$campsite <- ifelse(visits$currentvisitduration > tmin/timeinterval, TRUE, FALSE)

  # check for each visit if until the next visit at the same location there is a campsite in-between
  visits$campsiteinbetween <- sapply(seq_len(nrow(visits)), function(x){
    if(!is.na(visits$nextvisitsamelocationindex[x])){

      # get all visits in-between
      indexbetween <- (x:visits$nextvisitsamelocationindex[x])
      indexbetween <- indexbetween[-c(1, length(indexbetween))]

      # if there are visits in-between
      if(length(indexbetween) > 0){
        any(visits$campsite[indexbetween])
      }else{
        FALSE
      }
    }else{
      FALSE
    }
  })

  # set mergewithnext to FALSE
  visits$mergewithnext <- FALSE

  # get for each visit the number of repeated campsite visits
  visits <- trackvisitsSetNorepeatedcampsitevisits(currenttrackvisits = visits)

  # construct object of class trackvisits
  currenttrackvisits <- trackvisits(location = visits$location,
                                    group = NA,
                                    start = visits$start,
                                    end = visits$end,
                                    starttime = visits$starttime,
                                    endtime = visits$endtime,
                                    longitude = visits$longitude,
                                    latitude = visits$latitude,
                                    altitude = visits$altitude,
                                    currentvisitduration = visits$currentvisitduration,
                                    nextvisitduration = visits$nextvisitduration,
                                    nextvisitsamelocationindex = visits$nextvisitsamelocationindex,
                                    nextvisitsamelocation = visits$nextvisitsamelocation,
                                    nextvisitsamelocationduration = visits$nextvisitsamelocationduration,
                                    campsite = visits$campsite,
                                    campsiteinbetween = visits$campsiteinbetween,
                                    mergewithnext = visits$mergewithnext,
                                    norepeatedcampsitevisits = as.numeric(visits$norepeatedcampsitevisits)
  )

  # set attributes
  attr(currenttrackvisits, "coords") <- data.frame(longitude = currenttrack$longitude,
                                                   latitude = currenttrack$latitude,
                                                   altitude = currenttrack$altitude)
  # return currenttrackvisits
  return(currenttrackvisits)

}

