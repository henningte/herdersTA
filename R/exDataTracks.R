#'@importFrom Rdpack reprompt
#'@import spacetime
#'@import lubridate
NULL

#' Extracts data from a \code{\link[trajectories:Track-class]{Tracks}} object.
#'
#' \code{exDataTracks} extracts data in the \code{data} slot and
#' \code{time} slot of all \code{\link[trajectories:Track-class]{Track}} objects
#' of specified \code{\link[trajectories:Track-class]{Tracks}} object and merges
#' the values into a \code{data.frame} for each
#' \code{\link[trajectories:Track-class]{Track}} object
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{Tracks}} object.
#' @return A \code{data.frame} objects representing the merged slots
#' \code{data} and \code{time} of the \code{\link[trajectories:Track-class]{Track}}
#' objects of the specified \code{\link[trajectories:Track-class]{Tracks}}
#' object.
#' @seealso #
#' @examples #
#' @export
exDataTracks <- function(currenttracks){

  # create a list with the merged data
  df <- lapply(currenttracks@tracks, function(x){
    df <- x@data
    df$time <- as.POSIXct(x@time)
    df
  })

  # merge the individual data.frame objects
  df1 <- df[[1]]
  if(length(df) > 1){

    for(i in c(2:length(df))){
      df1 <- merge(df1, df[[i]], all = T)
    }

  }

  # order the entries according to the time
  df1 <- df1[order(df1$time),]

  # return the result
  return(df1)

}
