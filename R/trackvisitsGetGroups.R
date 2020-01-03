#' Groups Visits from Track Objects
#'
#' \code{trackvisitsGetGroups} defines groups of visits at the same location
#' based on (1) the duration between these visits, (2) the time of these visits
#' and (3) whether there is a campsite in-between. Additionally, the variabe
#' \code{mergewithnext} in the input \code{\link{trackvisits}} is defined, i.e.
#' it is given advice on visits to merge.
#'
#' @param trackvisits An object of class \code{\link{trackvisits}}.
#' @param tmin The minimum residence time at a specific location [s]
#' which is used to classify visits as campsites (if the residence
#' time at a specific location is larger than \code{tmin}) in
#' contrast to short-term visits of locations. The default is
#' \code{tmin = 345600}, i.e. 4 days.
#' @param timeinterval The number of seconds one data value (row) in
#' \code{currenttrack} covers.
#' @param tmaxinterstice A numeric value giving the maximum duration of an
#' interstice between two visits at the same location until which
#' these visits are merged to one group if there is no campsite visit
#' in-between.
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
#' @return An object of class \code{\link{trackvisits}}.
#'
#' @seealso \code{\link{trackvisits}}.
#' @examples #
#' @export
trackvisitsGetGroups <- function(trackvisits,
                                 tmin,
                                 timeinterval = 30*60,
                                 tmaxintersticenotvalid = NULL,
                                 tmaxinterstice) {

  # checks
  if(!(inherits(trackvisits, "trackvisits"))){
    stop("trackvisits must be of class trackvisits\n")
  }
  if(!(inherits(tmaxintersticenotvalid, "data.frame") || ncol(tmaxintersticenotvalid) == 2 || is.null(tmaxintersticenotvalid))){
    stop("tmaxintersticenotvalid must be a data.frame with two columns or NULL\n")
  }

  # check for each visit if nextvisitsamelocationduration < tmaxinterstice
  check_nextvisitsamelocationduration <- ifelse(trackvisits$nextvisitsamelocationduration * timeinterval < tmaxinterstice, TRUE, FALSE)

  # check for each visit if campsiteinbetween == FALSE
  check_campsiteinbetween <- !trackvisits$campsiteinbetween

  if(!is.null(tmaxintersticenotvalid)){

    # check for each time period in tmaxintersticenotvalid which visits end or start within this tmaxintersticenotvalid. check for each visit if the next visit at the same location is within tmaxintersticenotvalid
    indextmaxintersticenotvalids <- do.call("c", lapply(seq_len(nrow(tmaxintersticenotvalid)), function(x){

      # index of visits ending or starting in tmaxintersticenotvalid
      indexvisitsintmaxintersticenotvalid <- which((trackvisits$endtime >= tmaxintersticenotvalid$start[x] & trackvisits$endtime <= tmaxintersticenotvalid$end[x]) | (trackvisits$starttime >= tmaxintersticenotvalid$start[x] & trackvisits$starttime <= tmaxintersticenotvalid$end[x]))

      # index of visits in tmaxintersticenotvalid at the same location
      indexnextvisitsamelocationintmaxintersticenotvalid <- indexvisitsintmaxintersticenotvalid[which(duplicated(trackvisits$location[indexvisitsintmaxintersticenotvalid], fromLast = TRUE))]

    }))

    # set respective values in check_nextvisitsamelocationduration to TRUE
    check_nextvisitsamelocationduration[indextmaxintersticenotvalids] <- TRUE
  }

  # set trackvisits$mergewithnext
  trackvisits$mergewithnext <- ifelse(check_nextvisitsamelocationduration & check_campsiteinbetween, TRUE, FALSE)
  trackvisits$mergewithnext[is.na(trackvisits$mergewithnext)] <- FALSE

  # define group
  group <- 1
  for(x in seq_len(nrow(trackvisits))){

    if(trackvisits$mergewithnext[x]){
      trackvisits$group[x:trackvisits$nextvisitsamelocationindex[x]] <- group
      indexbetween <- (x:trackvisits$nextvisitsamelocationindex[x])
      indexbetween <- indexbetween[-c(1, length(indexbetween))]
      if(length(indexbetween) > 0){
        trackvisits$mergewithnext[indexbetween] <- TRUE
        trackvisits$nextvisitsamelocationindex[indexbetween] <- trackvisits$nextvisitsamelocationindex[x] - rev(seq_along(indexbetween)) + 1
      }
    }else{
      trackvisits$group[x] <- group
      group <- group + 1
    }

  }

  # return trackvisits
  return(trackvisits)

}
