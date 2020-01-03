#' Get the Number of Repeated Campsite Visits.
#'
#' \code{trackvisitsSetNorepeatedcampsitevisits} sets the number of repeated campsite visits
#' for each location of an object of class \code{\link{trackvisits}}. A visit is a repeated
#' campsite visit if (1) it is a campsite visit and (2) the previous visit is at a different
#' location.
#'
#' @param currenttrackvisits An object of class \code{\link{trackvisits}} for
#' which the variables \code{campsite} and \code{nextvisitsamelocation} are
#' defined.
#' @return An object of class \code{\link{trackvisits}} in which the number of
#' repeated campsite visits at the same location is set.
#'
#' @seealso \code{\link{trackvisits}}.
#' @examples #
#' @export
trackvisitsSetNorepeatedcampsitevisits <- function(currenttrackvisits){

  # checks
  if(!(inherits(currenttrackvisits, "trackvisits"))){
    stop("currenttrackvisits must be of class trackvisits\n")
  }

  # get for each visit the number of repeated campsite currenttrackvisits
  currenttrackvisits$norepeatedcampsitevisits <- NA
  repeatedcampsitevisits <- tapply(seq_len(nrow(currenttrackvisits)), currenttrackvisits$location, function(x){
    counter <- 1
    currentrepeatedvisits <- rep(NA, length(x))
    for(y in seq_along(x)){
      if(y == 1 && currenttrackvisits$campsite[x[y]]){
        currentrepeatedvisits[y] <- counter
      }else{
        if(!currenttrackvisits$nextvisitsamelocation[x[y]-1] && currenttrackvisits$campsite[x[y]]){
          counter <- counter + 1
          currentrepeatedvisits[y] <- counter
        }
      }

    }
    currentrepeatedvisits
  }, simplify = FALSE)
  lapply(seq_along(repeatedcampsitevisits), function(x){
    currenttrackvisits$norepeatedcampsitevisits[currenttrackvisits$location == names(repeatedcampsitevisits)[x]] <<- repeatedcampsitevisits[[x]]
  })

  # return currenttrackvisits
  return(currenttrackvisits)

}
