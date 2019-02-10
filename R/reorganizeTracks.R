#' @importFrom Rdpack reprompt
#' @importFrom spacetime STIDF
#' @import lubridate
#' @import trajectories
#' @import rgdal
#' @importFrom sp coordinates SpatialPoints
#' @import doParallel
#' @importFrom tidyr fill
NULL

#' Reorganises \code{\link[trajectories:Track-class]{Tracks}} objects.
#'
#' \code{reorganizeTracks} reorganises a \code{\link[trajectories:Track-class]{TracksCollection}}
#' object with each \code{\link[trajectories:Track-class]{Tracks}} object containing one
#' \code{\link[trajectories:Track-class]{Track}} object. The
#' \code{\link[trajectories:Track-class]{Track}} objects
#' covering roughly the same time period are reorganised by assigning data points to
#' the same overall time vector with half-hourly spaced data and including gaps as
#' new data points within each \code{\link[trajectories:Track-class]{Track}} object.
#' Gaps are filled with dummy values from the next value prior the gap or if there is
#' an ending gap, with the next value prior the ending gap.
#'
#' If there are more than one data point per interval and
#' \code{\link[trajectories:Track-class]{Track}} object, only the first data
#' point within this interval is retained.
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{TracksCollection}} object
#' with only one \code{\link[trajectories:Track-class]{Track}} object per
#' \code{\link[trajectories:Track-class]{Tracks}} object.
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel]{clusterCall}}.
#' @return A \code{\link[trajectories:Track-class]{Tracks}} object containing
#' the reorganised \code{\link[trajectories:Track-class]{Track}} objects of the
#' input A \code{\link[trajectories:Track-class]{Tracks}} object.
#' @seealso \code{\link{mergeTracksCollection}}, \code{\link{mergeTracks}}.
#' @examples #
#' @export
reorganizeTracks <- function(currenttracks,
                             interval = 60*30,
                             tz = "GMT",
                             crs = "+proj=longlat +ellps=WGS84",
                             cores = 1,
                             clcall = NULL){

  # extract the names
  currenttracksnames <- names(currenttracks@tracksCollection)

  # define a vector with time points equally spaced according to interval
  tstart <- lubridate::floor_date(min(currenttracks@tracksCollectionData$tmin), unit = "hours")
  tend <- lubridate::ceiling_date(max(currenttracks@tracksCollectionData$tmax), unit = "hours")
  times <- seq(from = tstart, by = interval, length.out = ceiling(as.numeric(difftime(tend, tstart, units = "secs"))/interval))

  # define a data.frame for time
  dftime <- data.frame(time = times)

  # define a funciton to round POSIXct to the next half hour
  round_date_halfhour <- function(date){

    # extract the minutes
    minutes <- as.numeric(strftime(date, format = "%M"))

    # get the minutes to add
    minutestoadd <- ifelse(minutes > 30, -(minutes-30), -minutes)
    minutestoadd[minutes == 30] <- 0

    # adjust the time
    rounddate <- lubridate::floor_date(x = date + minutestoadd * 60, unit = "minute")
  }

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(is.null(clcall) == F){
    clusterCall(cl, clcall)
  }
  on.exit(expr = stopCluster(cl))

  newcurrenttracks <- trajectories::TracksCollection(foreach(x = currenttracks@tracksCollection, .packages = c("lubridate", "tidyr", "trajectories", "spacetime", "sp"))%dopar%{

    # extract the corresponding Track object
    x <- x@tracks[[1]]

    # extract the data slot
    xdata <- x@data

    # extract the time slot and round it to times
    xdata$time <- round_date_halfhour(date = as.POSIXct(x@time))

    # extract the coordinates
    xdata$longitude <- sp::coordinates(x@sp)[,1]
    xdata$latitude <- sp::coordinates(x@sp)[,2]

    # get all time values in x that have no correspondence in dftime
    gaps <- which(!(dftime$time %in% xdata$time))

    # remove duplicates by retaining only the first value
    xdata <- xdata[!duplicated(xdata$time),]

    # merge xdata to dftime
    xdata <- merge(x = xdata, y = dftime, by = "time", all.y = TRUE)

    # flag gaps
    xdata$gap <- FALSE
    xdata$gap[gaps] <- TRUE

    # fill values of gaps with dummy values
    xdata <- tidyr::fill(xdata, seq_len(ncol(xdata)), .direction = "up")
    xdata <- tidyr::fill(xdata, seq_len(ncol(xdata)), .direction = "down")

    # create the reorganised Track object
    trajectories::Tracks(list(trajectories::Track(STIDF(sp = SpatialPoints(coords = xdata[,c("longitude", "latitude")], proj4string = CRS(proj4string(x))), time = xdata$time, data = xdata[,!names(xdata) %in% c("time", "longitude", "latitude")], endTime = xdata$time))))

  })

  # restore the names
  names(newcurrenttracks@tracksCollection) <- currenttracksnames

  # return newcurrenttracks
  return(newcurrenttracks)

}
