#'@importFrom Rdpack reprompt
#'@import trajectories
NULL

#' Reads data from a set of GPS track files.
#'
#' \code{loadTracks} reads data from all .csv files containing data on
#' GPS tracks within a specified directory. The function also removes
#' empty .csv files within the directory by calling the function
#' \code{\link{removeEmptyFiles}}.
#'
#' @param folder A character value representing the path to a folder with .csv files
#' containing data on GPS tracks.
#' @return A \code{\link[trajectories:Track-class]{Tracks}} object holding the data of the
#' GPS tracks.
#' @seealso \code{\link{removeEmptyFiles}}, \code{\link{readTrack}}.
#' @examples #
#' @export
loadTracks <- function(folder){

  # print message
  print(paste("Loading tracks from folder", folder, sep = " "),quote = F)

  # get filenames
  lst <- list.files(folder, pattern = "*.CSV", full.names = T)

  # get names of not epty files
  lst <- removeEmptyFiles(lst)

  # load the data and return a Tracks object
  Tracks(lapply(lst, readTrack))

}
