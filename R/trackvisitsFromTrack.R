#' @importFrom data.table rbindlist
#' @importFrom sp coordinates
NULL

#' Extracts Visits from Track Objects
#'
#' \code{trackvisitsFromTrack} extracts an object of class
#' \code{\link{trackvisits}} from an object of class
#' \code{\link[trajectories:Track-class]{Track}}.
#'
#' @param currenttrack An object of class \code{\link[trajectories:Track-class]{Track}}
#' with a variable \code{location} that is an id for locations. Gap values
#' must have the value \code{0} for \code{location}. \code{currenttrack}
#' must contain data values for equally spaced time intervals, i.e. each value
#' must correspond to a time interval of the same length. Additionally,
#' \code{currenttrack} must contain a numeric varaibel \code{altitude}.
#' @param tmin The minimum residence time at a specific location [s]
#' which is used to classify visits as campsites (if the residence
#' time at a specific location is larger than \code{tmin}) in
#' contrast to short-term visits of locations. The default is
#' \code{tmin = 345600}, i.e. 4 days.
#' @return An object of class \code{\link{trackvisits}}.
#'
#' @seealso \code{\link{trackvisits}}.
#' @examples #
#' @export
trackvisitsFromTrack <- function(currenttrack,
                                 tmin = 345600) {

  # checks
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack must be of class Track\n")
  }
  if(!("location" %in% colnames(currenttrack@data))){
    stop("currenttrack must contain a variable 'location'\n")
  }
  if(!(is.numeric(tmin) || tmin > 0)){
    stop("tmin must be a numeric value > 0\n")
  }

  # extract the time interval of adjacent values in currenttrack
  timeinterval <- as.numeric(difftime(time1 = as.POSIXct(currenttrack@time)[2],
                                      time2 = as.POSIXct(currenttrack@time)[1],
                                      units = "secs"))

  # extract the coordinates of currenttrack
  xcoords <- sp::coordinates(currenttrack@sp)

  # extract visits for each location
  visits <- data.table::rbindlist(lapply(unique(currenttrack$location), function(x){
    if(x != 0){
      currentblock <- identifyBlocksVariable(currenttrack = currenttrack,
                                             variable = "location",
                                             value = x)
      currentblock$location <- rep(x, nrow(currentblock))
      currentblock
    }
  }))

  # sort visits according to visits$start
  visits <- visits[order(visits$start),]

  # extract the respective start and end times
  visits$starttime <- as.POSIXct(currenttrack@time)[visits$start]
  visits$endtime <- as.POSIXct(currenttrack@time)[visits$end]

  # extract the respective median coordinates
  visits$longitude <- sapply(seq_len(nrow(visits)), function(x){
    median(xcoords[visits$start[x]:visits$end[x], 1])
  })
  visits$latitude <- sapply(seq_len(nrow(visits)), function(x){
    median(xcoords[visits$start[x]:visits$end[x], 2])
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
  attr(currenttrackvisits, "coords") <- data.frame(longitude = xcoords[,1],
                                                   latitude = xcoords[,2],
                                                   altitude = currenttrack$altitude)
  # return currenttrackvisits
  return(currenttrackvisits)

}

