#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Classifies Data Values of a \code{\link[trajectories]{Tracks}} object as Night or Day Values.
#'
#' \code{classifyNightTrack} creates labels for the data values
#' of a \code{\link[trajectories]{Track}} object marking
#' a data value as recorded during night or during day. The decision
#' boundaries are specified by the user.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object.
#' \code{currenttracks} must have a variable \code{time} indicating
#' the time of all data values.
#' @param night An integer vector with two elements:
#' \enumerate{
#'   \item The first element specifies the start hour of the night, e.g. \code{0}
#'   for 0 o'clock.
#'   \item The first element specifies the start hour of the night, e.g. \code{4}
#'   for 4 o'clock.
#' }
#' @return A \code{\link[trajectories]{Track}} object identical to \code{currenttrack}
#' with an attribute \code{night} representing a logical vector with a value for
#' each data value in \code{currenttrack} that is \code{TRUE} if a data value was
#' recorded during night and \code{FALSE} if it was recorded during day.
#' @seealso
#' @examples #
#' @export
classifyNightTrack <- function(currenttrack, night){

  # check if currenttrack is specified correctly
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack has to be a Track object\n")
  }

  # check if night is specified correctly
  if(!(is.numeric(night) && length(night) == 2 && all(sapply(night, function(x) x %in% 0:24)))){
    stop("night has to be an integer vector with two elements in [0;24]")
  }

  # extract the time information (hours) of the first Track object in currenttrack
  time <- as.numeric(strftime(currenttrack$time, "%H"))

  # classify the values of time as night (TRUE) or day (FALSE) and add the result as attribute night to currenttrack
  attributes(currenttrack)$night <- ifelse(time >= night[1] & time <= night[2], TRUE, FALSE)

  # return currenttrack
  return(currenttrack)

}
