#' @importFrom Rdpack reprompt
#' @import spacetime
#' @import lubridate
#' @import trajectories
#' @import rgdal
#' @import sp
#' @import doParallel
NULL

#' Reorganises \code{\link[trajectories]{Tracks}} objects.
#'
#' \code{reorganizeTracks} reorganises a \code{\link[trajectories]{Tracks}}
#' object with several \code{\link[trajectories]{Track}} objects
#' covering roughly the same time period by assigning data points to
#' the same overall time interval and including gaps as new data
#' points within each \code{\link[trajectories]{Track}} object.
#'
#' If there are more than one data point per interval and
#' \code{\link[trajectories]{Track}} object, only the first data
#' point within this interval is retained.
#'
#' @param currenttracks A \code{\link[trajectories]{Tracks}} object.
#' @param interval A numerical value representing the length of the
#' time interval to assign all data values to [s]. Default is
#' \code{interval = 60*30}, i.e. 30 minutes.
#' @param tz A character value indicating the time zone the temporal
#' information orresponds to. Default is \code{tz = "GMT"}.
#' @param cores An integer value representing the number of cores to
#' use in parallel computing.
#' @param clcall A function that is passed to
#' \code{\link[parallel]{clusterCall}}.
#' @return A \code{\link[trajectories]{Tracks}} object containing
#' the reorganised \code{\link[trajectories]{Track}} objects of the
#' input A \code{\link[trajectories]{Tracks}} object.
#' @seealso \code{\link{mergeTracksCollection}}, \code{\link{mergeTracks}}.
#' @examples #
#' @export
reorganizeTracks <- function(currenttracks,
                             interval = 60*30,
                             tz = "GMT",
                             cores = 1,
                             clcall = NULL){

  # define a vector with time points equally spaced according to interval
  tstart <- as.POSIXct(min(sapply(currenttracks@tracks, function(x){min(as.POSIXct(x@time))})), origin = "1970-01-01 00:00:00", tz = tz)
  tstart <- floor_date(tstart, unit = "hours")
  tend <- as.POSIXct(max(sapply(currenttracks@tracks, function(x){max(as.POSIXct(x@time))})), origin = "1970-01-01 00:00:00", tz = tz)
  tend <- ceiling_date(tend, unit = "hours")
  times <- seq(from = tstart, by = interval, length.out = ceiling(as.numeric(difftime(tend, tstart, units = "secs"))/interval))

  # define a matrix with time intervals
  timeintervals <- data.frame(tstart = times[-length(times)], tend = times[-1])

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(is.null(clcall) == F){
    clusterCall(cl, clcall)
  }
  clusterCall(cl, function(){library("lubridate")})
  clusterCall(cl, function(){library("sp")})
  clusterCall(cl, function(){library("rgdal")})
  clusterCall(cl, function(){library("trajectories")})
  clusterCall(cl, function(){library("spacetime")})
  clusterExport(cl = cl, varlist = list("currenttracks", "timeintervals", "tz"), envir = environment())

  # reorganize track objects by including data points for gaps
  newtracks <- foreach(track_i = seq_along(currenttracks@tracks))%dopar%{

    # convert x@time (xts) for use in cluster
    timetrack <- as.POSIXct(currenttracks@tracks[[track_i]]@time)

    # define columns lon and lat of currenttracks
    currenttracks@tracks[[track_i]]@data$lon <- coordinates(currenttracks@tracks[[track_i]]@sp)[,1]
    currenttracks@tracks[[track_i]]@data$lat <- coordinates(currenttracks@tracks[[track_i]]@sp)[,2]

    # force to numeric
    currenttracks@tracks[[track_i]]@data$lon <- as.numeric(as.character(currenttracks@tracks[[track_i]]@data$lon))
    currenttracks@tracks[[track_i]]@data$lat <- as.numeric(as.character(currenttracks@tracks[[track_i]]@data$lat))

    # define which row to replicate if there is a gap at the first time interval
    listcond <- list()

    # clusterExport(cl = cl, varlist = list("currenttracks", "timeintervals", "tz", "track_i", "timetrack", "listcond"), envir = environment())

    newtrack <-
      do.call(rbind, lapply(c(1:nrow(timeintervals)), function(timeinterval_i){

      # convert timeintervals[timeinterval_i,]
      timeintervals1 <- as.POSIXct(as.numeric(timeintervals[timeinterval_i,]), origin = "1970-01-01 00:00:00", tz = tz)

      # define condition
      cond <- which(timetrack >= timeintervals1[1] & timetrack <= timeintervals1[2])
      if(length(cond) != 0){
        listcond <- c(listcond, cond[1])
      }
      if(length(listcond) > 0){
        condprev <- listcond[[which(lapply(listcond, function(y){length(y)}) > 0)[length(which(lapply(listcond, function(y){length(y)}) > 0))]]]
      }else{
        condprev <- 1
      }

      # define entry_i of the new Track object
      if(length(cond) == 0 && timeinterval_i == 1){
        newpoint <- c(as.vector(currenttracks@tracks[[track_i]]@data[1,]), T)
      }
      if(length(cond) == 0 && timeinterval_i != 1){
        newpoint <- c(as.vector(currenttracks@tracks[[track_i]]@data[condprev,]), TRUE)
      }
      if(length(cond) == 1){
        newpoint <- c(as.vector(currenttracks@tracks[[track_i]]@data[cond,]), FALSE)
      }

      # evaluate if exactly one data point per time interval
      if(length(cond) > 1){
        warning(paste0(length(cond), " data points per interval ", timeinterval_i, " in a Track."))
        newpoint <- c(as.vector(currenttracks@tracks[[track_i]]@data[cond[1],]), FALSE)
      }

      # convert newpoint to a vector
      newpoint <- do.call(c, newpoint)

      # change time information (connections with zero duration are not allowed)
      if(newpoint[length(newpoint)] == T){
        newpoint["time"] <- mean(as.numeric(timeintervals[timeinterval_i,]))
      }

      # return result
      return(newpoint)

    }))

    # remove rownames
    rownames(newtrack) <- NULL

    # convert to dataframe
    newtrack <- as.data.frame(newtrack, stringsAsFactors = FALSE)

    # add name for new variable
    names(newtrack)[ncol(newtrack)] <- "gap"

    # redefine the index (relating to time in ascending order)
    newtrack$INDEX <- seq(1, nrow(newtrack))

    # extract gaps and adjust time information (conections of zero duration are not allowed)
    for(row_i in c((nrow(newtrack)-1):2)){
      if(newtrack$time[row_i] == newtrack$time[row_i - 1]){
        newtrack$time[row_i-1] <- as.POSIXct(as.numeric(newtrack$time[row_i]) - interval, origin = "1970-01-01 00:00:00", tz = tz)
      }
    }

    # convert several attributes
    #newtrack[,c(1, 3, 4, 7, 8, 16, 17)] <- apply(newtrack[,c(1, 3, 4, 7, 8, 16, 17)], 2, function(x){as.numeric(as.character(x))})
    newtrack$lon <- as.numeric(as.character(newtrack$lon))
    newtrack$lat <- as.numeric(as.character(newtrack$lat))
    newtrack$time <- as.POSIXct(as.numeric(as.character(newtrack$time)), origin = "1970-01-01 00:00:00", tz = tz)

    # return result
    return(newtrack)

  }

  # create SpatialPoints objects from newtracks
  newtrackssp <-
    parLapply(cl, newtracks, function(x){

      # create sp object
      newtracksp <- SpatialPoints(coords = data.frame(lon = x$lon, lat = x$lat), proj4string = CRS(
        "+proj=longlat +zone=46 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      ))

      # return result
      return(newtracksp)

    })

  # create new Track objects
  clusterExport(cl = cl, varlist = list("timeintervals", "newtracks", "newtrackssp"), envir = environment())
  newtracks1 <- parLapply(cl, seq_along(newtracks), function(track_i){

    a <- Track(STIDF(sp = newtrackssp[[track_i]], time = as.POSIXct(timeintervals[,1]) , data = newtracks[[track_i]], endTime = as.POSIXct(timeintervals[,2])))

  })
  newtracks1 <- Tracks(newtracks1)

  # stop cluster
  stopCluster(cl)

  # return result
  return(newtracks1)

}
