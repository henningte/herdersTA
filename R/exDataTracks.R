#' Extracts data from a Tracks object.
#'
#' \code{exDataTracks} extracts data in the \code{data} slot of all
#' \code{\link[trajectories:Track-class]{Track}} objects
#' of specified \code{\link[trajectories:Track-class]{Tracks}} object and merges
#' the values into a \code{data.frame} for each
#' \code{\link[trajectories:Track-class]{Track}} object
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{Tracks}} object.
#' @return A \code{data.frame} objects representing the merged slots
#' \code{data} of the \code{\link[trajectories:Track-class]{Track}}
#' objects of the specified \code{\link[trajectories:Track-class]{Tracks}}
#' object.
#' @seealso #
#' @examples #
#' @export
exDataTracks <- function(currenttracks){

  # extract the time information
  time <- do.call("c", lapply(currenttracks@tracks, function(x) as.POSIXct(x@time)))

  # merge the individual data slots
  df <- data.table::rbindlist(lapply(currenttracks@tracks, function(x){
    x@data
  }), use.names = TRUE, fill = TRUE)

  # order the entries according to the time
  df[order(time),]

}
