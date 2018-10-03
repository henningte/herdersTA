#' @importFrom Rdpack reprompt
NULL

#' Redefines indices based on a time vector.
#'
#' \code{redefineIndices} redefines a column with index values (integer)
#' according to a column with time information in a  \code{data.frame}
#' object, i.e. new indices will be assigned to each set of rows with
#' the same index in relation to their order in time.
#'
#' @param df A \code{data.frame} object with a column representing
#' indices and a column representing time information as \code{POSIXct}
#' values.
#' @param indices A character value giving the name of the column of
#' \code{df} which contains the time values.
#' @param time indices A character value giving the name of the column of
#' \code{df} which contains the index values.
#' @param notchange An integer vector specifying which original index values
#' should not be changed. Default is \code{notchange = NULL}, i.e. all indices
#' will be changed.
#' @return A \code{data.frame} object with the redefined index values.
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{extractClutersBuffer}},
#' \code{\link{fillGapTrack}}, \code{\link{fillGapTracks}},
#' \code{\link{locationsTrack}}.
#' @examples #
#' @export
redefineIndices <- function(df, indices, time, notchange = NULL
){

  # get order of time information
  order_time <- order(df[time])

  # define vectors to store information on already processes values in
  done <- NULL

  # define start value for new indices
  iter <- 1
  while(iter %in% notchange){
    iter <- iter+1
  }

  # redefine indices of locations (according to arrival time)
  index_new <- rep(0, nrow(df))
  for(i in c(1:nrow(df))){
    if(i %in% done || df[indices][i,] %in% notchange){
      next()
    }else{
      done <- c(done, which(df[indices] == df[indices][i,]))
      index_new[which(df[indices] == df[indices][i,])] <- iter
      iter <- iter + 1
      while(iter %in% notchange){
        iter <- iter + 1
      }
    }
  }

  # redefine df[indices]
  df[indices] <- index_new

  # return result
  return(df)

}
