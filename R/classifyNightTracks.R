#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Classifies Data Values of a \code{\link[trajectories]{Tracks}} object as Night or Day Values.
#'
#' \code{classifyNightTracks} creates labels for the data values
#' of a \code{\link[trajectories]{Tracks}} object with
#' \code{\link[trajectories]{Track}} objects with the same time
#' vectors (i.e. data values for the same time interval) marking
#' a data value as recorded during night or during day. The decision
#' boundaries are specified by the user.
#'
#' @param currenttracks A \code{\link[trajectories]{Tracks}} object
#' with \code{\link[trajectories]{Track}} objects with the same time
#' vectors (i.e. data values for the same time interval) (as returned
#' by \code{\link{reorganizeTracks}}). \code{currenttracks} must have
#' a variable \code{time} indicating the time of all data values.
#' @param night An integer vector with two elements:
#' \enumerate{
#'   \item The first element specifies the start hour of the night, e.g. \code{0}
#'   for 0 o'clock.
#'   \item The first element specifies the start hour of the night, e.g. \code{4}
#'   for 4 o'clock.
#' }
#' @return A \code{\link[trajectories]{Tracks}} identical to \code{currenttracks}
#' with an attribute \code{night} representing a logical vector with a value for
#' each data value in a \code{\link[trajectories]{Track}} object in
#' \code{currenttracks} that is \code{TRUE} if a data value was recorded during night
#' and \code{FALSE} if it was recorded during day.
#' @seealso
#' @examples #
#' @export
classifyNightTracks <- function(currenttracks, night){

  # check if currenttracks is specified correctly
  if(!inherits(currenttracks, "Tracks")){
    stop("currenttracks has to be a Tracks object\n")
  }

  # check if night is specified correctly
  if(!(is.numeric(night) && length(night) == 2 && all(sapply(night, function(x) x %in% 0:24)))){
    stop("night has to be an integer vector with two elements in [0;24]")
  }

  # extract the time information (hours) of the first Track object in currenttracks
  time <- as.numeric(strftime(currenttracks@tracks$time, "%H"))

  # classify the values of time as night (TRUE) or day (FALSE) and add the result as attribute night to currenttracks
  attributes(currenttracks)$night <- ifelse(time >= night[1] & time <= night[2], TRUE, FALSE)

  # return currenttracks
  return(currenttracks)

}
