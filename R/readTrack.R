#'@importFrom Rdpack reprompt
#'@import trajectories
NULL

#' Reads data from an individual GPS track file.
#'
#' \code{readTrack} reads data from a .csv file containing data on an
#' individual GPS track.
#'
#' @param fname A character value representing the file name of the .csv file
#' containing the GPS data to read.
#' @return A \code{\link[trajectories:Track-class]{Track}} object holding the data of the
#' GPS track.
#' @seealso \code{\link{removeEmptyFiles}}.
#' @examples #
#' @export
readTrack <- function(fname){

  # function in order to convert numerical time information to POSIXct
  dt2POSIX = function(dt, tm) {
    stopifnot(is.numeric(dt) && is.numeric(tm))
    as.POSIXct(paste0(
      2000 + dt %/% 10000, "-",
      (dt %/% 100) %% 100, "-",
      dt %% 100, " ",
      tm %/% 10000, ":",
      tm %/% 100 %% 100, ":",
      tm %% 100), tz ="GMT")
  }

  # print message
  print(paste("reading file", fname))

  # import the .csv file
  x <- read.csv(fname, skipNul = TRUE) # most files contain embedded nuls

  # reformatting
  x <- x[!is.na(x$DATE),]
  x$lat = as.numeric(sub("N", "", x$LATITUDE.N.S))
  x$lon = x$LONGITUDE.E.W
  tm = dt2POSIX(x$DATE, x$TIME)
  pts = SpatialPoints(cbind(x$lon, x$lat), CRS("+proj=longlat +ellps=WGS84"))
  st = STIDF(pts, tm, x)

  # convert to Track object
  Track(st)

}
