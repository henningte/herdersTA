#' Check if the Next Visit is at the Same Location.
#'
#' \code{trackvisitsSetNextvisitsamelocationindex} sets the index of the next visit
#' at the same location in an object of class \code{\link{trackvisits}} is at the
#' same location.
#'
#' @param currenttrackvisits An object of class \code{\link{trackvisits}}.
#' @return An object of class \code{\link{trackvisits}} in which
#' nextvisitsamelocationindex is defined.
#'
#' @seealso \code{\link{trackvisits}}.
#' @examples #
#' @export
trackvisitsSetNextvisitsamelocationindex <- function(currenttrackvisits){

  # checks
  if(!(inherits(currenttrackvisits, "trackvisits"))){
    stop("currenttrackvisits must be of class trackvisits\n")
  }

  # get for each visit the row index of the next visit at the same location
  currenttrackvisits$nextvisitsamelocationindex <- sapply(seq_len(nrow(currenttrackvisits)), function(x){

    # get the current location
    currentlocation <- currenttrackvisits$location[x]

    # get the row index of the next visit at the same location
    nextvisitssamelocationindex <- which(currenttrackvisits$location[-seq_len(x)] == currentlocation)[1]+x

    # compute the duration to this visit
    if(length(nextvisitssamelocationindex) > 0){
      nextvisitssamelocationindex
    }else{
      NA
    }

  })

  # return currenttrackvisits
  return(currenttrackvisits)

}
