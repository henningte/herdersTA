#' Merges Grouped Visits from Track Objects
#'
#' \code{trackvisitsMergeGroups} merges groups of visits at the same location.
#' For this, an object of class \code{\link{trackvisits}} has to be processed
#' with \code{\link{trackvisitsGetGroups}} first or the variable
#' \code{trackvisits$group} has to be defined as desired.
#'
#' @param currenttrackvisits An object of class \code{\link{trackvisits}} for
#' which groups have been defined.
#' @param tmin The minimum residence time at a specific location [s]
#' which is used to classify visits as campsites (if the residence
#' time at a specific location is larger than \code{tmin}) in
#' contrast to short-term visits of locations. The default is
#' \code{tmin = 345600}, i.e. 4 days.
#' @param timeinterval The number of seconds one data value (row) in
#' \code{currenttrack} covers.
#' @param keepgroup A logical value indicating if the input group values should
#' be retained for each group (\code{keepgroup = TRUE}) or group should be reset
#' to \code{NA} (\code{keepgroup = FALSE}).
#' @return An object of class \code{\link{trackvisits}} in which all
#' visits belonging to the same group are merged into one visit. The variables
#' of \code{currenttrackvisits} are changed as:
#' \describe{
#'   \item{\code{location}}{not changed (location of the first visit of the group).}
#'   \item{\code{group}}{if \code{keepgroup = TRUE}, the respective input group value,
#'   if \code{keepgroup = FALSE} reset to \code{NA}.}
#'   \item{\code{start}}{value of \code{start} of the first visit of the group.}
#'   \item{\code{end}}{value of \code{end} of the last visit of the group.}
#'   \item{\code{starttime}}{value of \code{starttime} of the first visit of the group.}
#'   \item{\code{endtime}}{value of \code{endtime} of the last visit of the group.}
#'   \item{\code{currentvisitduration}}{updated.}
#'   \item{\code{nextvisitduration}}{updated.}
#'   \item{\code{nextvisitsamelocationduration}}{updated.}
#'   \item{\code{nextvisitsamelocation}}{updated.}
#'   \item{\code{campsite}}{updated.}
#'   \item{\code{campsiteinbetween}}{updated.}
#'   \item{\code{mergewithnext}}{reset to \code{NA}.}
#' }
#'
#' @seealso
#' \code{\link{trackvisits}},
#' \code{\link{plot.trackvisits}},
#' \code{\link{trackvisitsFromTrack}},
#' \code{\link{trackvisitsGetGroups}},
#' \code{\link{trackvisitsSetNextvisitsamelocationindex}},
#' \code{\link{trackvisitsSetNorepeatedcampsitevisits}}.
#' @examples #
#' @export
trackvisitsMergeGroups <- function(currenttrackvisits,
                                   tmin,
                                   timeinterval = 30*60,
                                   keepgroup = FALSE) {

  # checks
  if(!(inherits(currenttrackvisits, "trackvisits"))){
    stop("currenttrackvisits must be of class trackvisits\n")
  }
  if(!(all(!is.na(currenttrackvisits$group)))){
    stop("group must not contain NA values\n")
  }
  if(!(is.logical(keepgroup) || length(keepgroup) != 1)){
    stop("keepgroup must be a numeric value\n")
  }

  # merge visits
  visits <-
    do.call(rbind, tapply(seq_len(nrow(currenttrackvisits)), currenttrackvisits$group, function(x){

      # define group
      if(keepgroup){
        group <- currenttrackvisits$group[x[1]]
      }else{
        group <- NA
      }

      trackvisits(location = currenttrackvisits$location[x[1]],
                  group = group,
                  start = currenttrackvisits$start[x[1]],
                  end = currenttrackvisits$end[x[length(x)]],
                  starttime = currenttrackvisits$starttime[x[1]],
                  endtime = currenttrackvisits$endtime[x[length(x)]],
                  longitude = as.numeric(NA),
                  latitude = as.numeric(NA),
                  altitude = as.numeric(NA),
                  currentvisitduration = currenttrackvisits$end[x[length(x)]] - currenttrackvisits$start[x[1]],
                  nextvisitduration = currenttrackvisits$start[x[length(x)]+1] - currenttrackvisits$end[x[length(x)]],
                  nextvisitsamelocationindex = NA,
                  nextvisitsamelocationduration = NA,
                  nextvisitsamelocation = currenttrackvisits$location[x[length(x)]+1] == currenttrackvisits$location[x[1]],
                  campsite = (currenttrackvisits$end[x[length(x)]] - currenttrackvisits$start[x[1]])*timeinterval >= tmin,
                  campsiteinbetween = FALSE,
                  mergewithnext = FALSE,
                  norepeatedcampsitevisits = as.numeric(NA)
      )

    }, simplify = FALSE))

  # extract the respective median coordinates
  visits$longitude <- sapply(seq_len(nrow(visits)), function(x){
    median(attr(currenttrackvisits, "coords")$longitude[visits$start[x]:visits$end[x]])
  })
  visits$latitude <- sapply(seq_len(nrow(visits)), function(x){
    median(attr(currenttrackvisits, "coords")$latitude[visits$start[x]:visits$end[x]])
  })
  visits$altitude <- sapply(seq_len(nrow(visits)), function(x){
    median(attr(currenttrackvisits, "coords")$altitude[visits$start[x]:visits$end[x]])
  })

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

  # get for each visit the number of repeated campsite visits
  visits <- trackvisitsSetNorepeatedcampsitevisits(currenttrackvisits = visits)

  # restore the attributes of currenttrackvisits
  attributes(visits)$coords <- attributes(currenttrackvisits)$coords

  # return visits
  return(visits)

}
