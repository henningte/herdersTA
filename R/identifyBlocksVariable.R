#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Identifies data value groups within a \code{data.frame} column.
#'
#' \code{identifyBlocksVariable} identifies blocks of rows in the
#' \code{data} slot of a \code{\link[trajectories]{Track}} object
#' or \code{data.frame} object
#' based on a grouping variable and a value for this variable. A
#' block is defined as a set of directly subsequent rows with the
#' same value for the specified variable.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object (or
#' \code{data.frame} object) with a boolean column \code{gap} in
#' \code{currenttrack@data}. Data values have to be regularly spaced (may
#' be achieved for example with \code{\link{reorganizeTracks}}).
#' @param variable A character value indicating the name of a
#' variable in \code{currenttrack@data} to group for.
#' @param value A value of the \code{currenttrack@data$variable} for
#' which to identify blocks of subsequent rows.
#' @return A \code{matrix} with two columns and rows for each identified
#' block. The first column indicates the row of \code{currenttrack@data}
#' where the block begins and the second column indicates the row of
#' \code{currenttrack@data} where the block ends or \code{NULL} if
#' \code{value} does not exist for \code{variable}.
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{extractClutersBuffer}},
#' \code{\link{redefineIndices}}, \code{\link{fillGapTrack}},
#' \code{\link{fillGapTracks}}, \code{\link{locationsTrack}}.
#' @examples #
#' @export
identifyBlocksVariable <- function(currenttrack, variable, value){

  # get indices of entries equal to the specified value
  whichvalue <- ifelse(inherits(currenttrack, "data.frame"), which(currenttrack[variable] == value), which(currenttrack@data[variable] == value))

  # identify blocks of the value within currenttrack
  if(length(whichvalue) == 0){
    blocksvalue1 <- NULL
  }else{
    if(length(whichvalue) > 1){
      blocksvalue <- whichvalue[-1] - whichvalue[-length(whichvalue)]
      blocksvalue1 <- NULL
      block <- NULL
      for(i in seq_along(blocksvalue)){

        # start of first block
        if(i == 1){
          block <- whichvalue[i]
        }

        # end of last block
        if(i == length(blocksvalue)){
          block <- c(block, whichvalue[i+1])
          blocksvalue1 <- rbind(blocksvalue1, block)
        }

        # end and start of intermediate block
        if(length(block) == 1 && blocksvalue[i] != 1){
          block <- c(block, whichvalue[i])
          blocksvalue1 <- rbind(blocksvalue1, block)
          block <- whichvalue[i+1]
        }else{
          next
        }

      }
    }else{
      blocksvalue1 <- matrix(rep(whichvalue, 2), nrow = 1, ncol = 2)
    }
  }

  # return result
  return(blocksvalue1)

}
