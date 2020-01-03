#' Finds non-empty .csv files in a directory.
#'
#' \code{removeEmptyFiles} returns the names of non-empty .csv files within a
#' directory by ignoring empty .csv files.
#'
#' @param listname A list of file names of the correpsonding GPS track files that
#' should be chekced.
#' @return A list of file names of the correpsonding GPS track files that are not
#' empty.
#' @seealso \code{\link{readTrack}}, \code{\link{removeEmptyFolders}}.
#' @examples #
#' @export
removeEmptyFiles <- function(listname){

  # create empty character vector to store names of empty files in
  remove <- character()

  # function in order to check which files are empty
  checkFile <- function(fname){
    if (file.size(fname) == 0){
      append(remove,fname)
    } else{
      append(remove,NA)
    }

  }

  # check which csv files are empty
  remove <- lapply(listname,checkFile)
  remove <- remove[!is.na(remove)]

  # print message
  print("Ignoring empty files:", quote=F)
  print(paste(remove), print.na = "", quote=F)

  # return names of not empty csv files
  listname[!listname %in% remove]

}
