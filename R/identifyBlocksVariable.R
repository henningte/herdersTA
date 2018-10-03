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
#' @param track A \code{\link[trajectories]{Track}} object (or
#' \code{data.frame} object) with a boolean column \code{gap} in
#' \code{track@data}. Data values have to be regularly spaced (may
#' be achieved for example with \code{\link{reorganizeTracks}}).
#' @param variable A character value indicating the name of a
#' variable in \code{track@data} to group for.
#' @param value A value of the \code{track@data$variable} for
#' which to identify blocks of subsequent rows.
#' @return A \code{matrix} with two columns and rows for each identified
#' block. The first column indicates the row of \code{track@data} where
#' the block begins and the second column indicates the row of
#' \code{track@data} where the block ends or \code{NULL} if \code{value}
#' does not exist for \code{variable}.
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{extractClutersBuffer}},
#' \code{\link{redefineIndices}}, \code{\link{fillGapTrack}},
#' \code{\link{fillGapTracks}}, \code{\link{locationsTrack}}.
#' @examples #
#' @export
identifyBlocksVariable <- function(track, variable, value){

  # get indices of entries representing gaps
  if(inherits(track, "data.frame")){
    which.gaps <- which(track[variable] == value)
  }else{
    which.gaps <- which(track@data[variable] == value)
  }

  # identify blocks of gaps within track
  if(length(which.gaps) == 0){
    blocks.gaps1 <- NULL
  }else{
    if(length(which.gaps) > 1){
      blocks.gaps <- which.gaps[-1] - which.gaps[-length(which.gaps)]
      blocks.gaps1 <- NULL
      block <- NULL
      for(i in seq_along(blocks.gaps)){

        # start of first block
        if(i == 1){
          block <- which.gaps[i]
        }

        # end of last block
        if(i == length(blocks.gaps)){
          block <- c(block, which.gaps[i+1])
          blocks.gaps1 <- rbind(blocks.gaps1, block)
        }

        # end and start of intermediate block
        if(length(block) == 1 && blocks.gaps[i] != 1){
          block <- c(block, which.gaps[i])
          blocks.gaps1 <- rbind(blocks.gaps1, block)
          block <- which.gaps[i+1]
        }else{
          next
        }

      }
    }else{
      blocks.gaps1 <- matrix(rep(which.gaps, 2), nrow = 1, ncol = 2)
    }
  }

  # return result
  return(blocks.gaps1)

}
