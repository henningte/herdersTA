#' Finds non-empty folders in a directory.
#'
#' \code{removeEmptyFolders} returns the names of non-empty folders within a
#' directory by ignoring empty folders.
#'
#' @param dirlist A list of folder names that should be chekced.
#' @return A list of folder names that are not empty.
#' @seealso \code{\link{removeEmptyFiles}}.
#' @examples #
#' @export
removeEmptyFolders <- function(dirlist){

  # create empty character vector to store names of empty folders in
  remove <- character()

  # function in order to check which folders are empty
  checkDir = function(dirname){
    if (length(list.files(dirname, pattern="*.CSV", full.names = T)) == 0){
      append(remove,dirname)
    } else{
      append(remove,NA)
    }

  }

  # check which folders are empty
  remove = lapply(dirlist, checkDir)
  remove = remove[!is.na(remove)]

  # print message
  print("Ignoring empty folders:", quote=F)
  print(paste(remove), print.na = "", quote=F)

  # return names of not empty folders
  dirlist[! dirlist %in% remove]

}
