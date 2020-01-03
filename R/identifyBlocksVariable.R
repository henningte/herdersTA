#' @importFrom data.table data.table rbindlist rleidv
NULL

#' Identifies data value groups within a \code{data.frame} column.
#'
#' \code{identifyBlocksVariable} identifies blocks of rows in the
#' \code{data} slot of a \code{\link[trajectories:Track-class]{Track}} object
#' or \code{data.frame} object
#' based on a grouping variable and a value for this variable. A
#' block is defined as a set of directly subsequent rows with the
#' same value for the specified variable.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object (or
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
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{extractClustersBuffer}},
#' \code{\link{redefineIndices}}, \code{\link{fillGapTrack}},
#' \code{\link{fillGapTracks}}, \code{\link{locationsTrack}}.
#' @examples #
#' @export
identifyBlocksVariable <- function(currenttrack,
                                   variable,
                                   value) {

  # extract the data.frame if necessary
  if(!inherits(currenttrack, "data.frame")){
    currenttrack <- currenttrack@data
  }

  # get  a grouping vector for the variable
  groups <- data.table::rleidv(currenttrack, variable)

  # get an index variable for the value of variable
  indexvariablevalue <- which(ifelse(currenttrack[variable] == value, TRUE, FALSE))

  # extract the respective block starts and ends
  data.table::rbindlist(tapply(seq_len(nrow(currenttrack))[indexvariablevalue], groups[indexvariablevalue], function(x){
    data.table::data.table(start = x[1],
                           end = x[length(x)],
                           stringsAsFactors = FALSE)
  }, simplify = FALSE))

}
