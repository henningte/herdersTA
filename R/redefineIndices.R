#' Redefines indices based on a time vector.
#'
#' \code{redefineIndices} redefines the values of a numeric vector
#' containing indices (integers) as group indices so that groups
#' recieve values in the order their first value occurs.
#'
#' @param x A numeric vector with integers as group indices.
#' @param notchange An integer vector specifying which original index values
#' should not be changed. Default is \code{notchange = NULL}, i.e. all indices
#' will be changed.
#' @return A \code{data.frame} object with the redefined index values.
#' @seealso #.
#' @examples #
#' @export
redefineIndices <- function(x,
                            notchange = NULL) {

  # checks
  if(!(is.numeric(x) || all(x%%1==0))){
    stop("x must be a numeric vector with integers\n")
  }

  # get first element for each gorup in x
  xgroups <- data.frame(old = x[!duplicated(x)],
                        stringsAsFactors = FALSE)

  # define new groupings (times 2 to ensure that no notchange is covered)
  newgroups <- seq_len(nrow(xgroups)*2)

  # discard all values in notchange
  newgroups <- newgroups[!(newgroups %in% notchange)]

  # get an index of xgroups$old in notchange
  xgroups$notchange <- ifelse(xgroups$old %in% notchange, TRUE, FALSE)

  # insert the new group values
  xgroups$new <- rep(NA, nrow(xgroups))
  xgroups$new[!xgroups$notchange] <- newgroups[seq_along(which(!xgroups$notchange))]
  xgroups$new[xgroups$notchange] <- xgroups$old[xgroups$notchange]

  # replace the original values
  sapply(x, function(y){
    xgroups$new[xgroups$old == y]
  })

}
