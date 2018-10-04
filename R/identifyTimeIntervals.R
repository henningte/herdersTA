#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Assigns values of GPS tracks to time intervals.
#'
#' \code{identifyTimeIntervals} assign each data value (row) in
#' the \code{data} slot of a \code{\link[trajectories]{Track}}
#' object to a user specified time interval.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object with a
#' \code{POSIXct} column \code{time} in \code{currenttrack@data}. Data
#' values have to be regularly spaced (may be achieved for example with
#' \code{\link{reorganizeTracks}}).
#' @param timeinterval One of \code{"year"}, \code{"month"},
#' \code{"tendayinterval"}, \code{"day"} or \code{"all"}. If
#' \code{timeinterval = "year"}, data values will be classified on a
#' yearly resolution. If \code{timeinterval = "month"}, data values
#' will be classified on a monthly resolution. If
#' \code{timeinterval = "tendayinterval"}, data values will be classified
#' on a ten-day interval resolution (i.e. assigned to fixed ten-day intervals).
#' Each month has three ten-day intervals
#' with the last having varying lengths of 11 to 8 days (depending on the
#' month and the occurance of leap years). If \code{timeinterval = "day"},
#' data values will be classified on a daily resolution. All time intervals
#' that are not completely covered by \code{currenttrack} (i.e. potentially
#' at the start and end of \code{currenttrack}) are handled as if they would
#' represent the whole specified time interval. If \code{timeintervall = "all"},
#' all data values will be assigned to one class. Default is
#' \code{timeinterval = "month"}.
#' @return An integer vector with a length equal to
#' \code{nrow(currenttrack@data)} with the same value for each data value
#' corresponding to the same time interval.
#' @seealso \code{\link{removeDataTrack}}, \code{\link{removeDataTracks}},
#' \code{\link{nogapDurationTrack}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
identifyTimeIntervals <- function(currenttrack, timeinterval = "month"){

  # assign data values to time intervals
  switch(timeinterval,
         all = {
           timeinterval <- assignall(currenttrack)
         },
         year = {
           timeinterval <- assignyear(currenttrack)
         },
         month = {
           timeinterval <- assignmonth(currenttrack)
         },
         tendayinterval = {
           timeinterval <- assigntendayinterval(currenttrack)
         },
         day = {
           timeinterval <- assignday(currenttrack)
         }
  )

}

# function in order to classify the whole time period as one group
assignall <- function(currenttrack){
  timeinterval <- rep(1, length(currenttrack@data$time))
}

# function in order to classify on a yearly resolution
assignyear <- function(currenttrack){

  # get unique years
  timeinterval <- rep(0, length(currenttrack@data$time))
  years <- strftime(currenttrack@data$time, format = "%Y")
  iter <- 1
  for(year_i in unique(years)){
    timeinterval[which(years == years_i)] <- iter
    iter <- iter + 1
  }

  return(timeinterval)
}

# function in order to classify on a monthly resolution
assignmonth <- function(currenttrack){

  # get unique months
  timeinterval <- rep(0, length(currenttrack@data$time))
  months <- strftime(currenttrack@data$time, format = "%y-%m")
  iter <- 1
  for(month_i in unique(months)){
    timeinterval[which(months == month_i)] <- iter
    iter <- iter + 1
  }

  return(timeinterval)

}

# function in order to classify on a fixed ten-day interval resolution
assigntendayinterval <- function(currenttrack){

  # get the years covered
  years <- unique(strftime(currenttrack@data$time, format = "%Y"))

  # get the number of days in each year
  days <- list()
  for(year_i in seq_along(years)){
    days[[year_i]] <- seq(as.POSIXct(paste0(years[year_i], "-01-01"), tz = attr(currenttrack@data$time, "tzone")), as.POSIXct(paste0(years[year_i], "-12-31"), tz = attr(currenttrack@data$time, "tzone")), "day")
  }

  # create a list with indices for days
  tendayintervals <- lapply(seq_along(days), function(y){

    # extract months
    months <- strftime(days[[y]], format = "%y-%m")

    # define an additional offset value if data for several years exist there
    offset <- (y - 1) * 36

    # get a vector with ten day-intervals
    tdi <- do.call(c, as.vector(sapply(seq_along(table(months)), function(x){

      # get indices of ten day-intervals
      if(which(names(table(months)) == names(table(months)[x])) == 1){
        indices <- 1:3
      }else{
        indices <- seq((which(names(table(months)) == names(table(months)[x]))-1) * 3 + 1, (which(names(table(months)) == names(table(months)[x]))-1) * 3 + 3)
      }

      c(rep(indices[1], 10), rep(indices[2], 10), rep(indices[3], table(months)[x] - 20))

    }))) + offset

  })

  # assign each day to a ten day-interval
  tendayintervals <-
    do.call(c, sapply(seq_along(days), function(x){
      names(tendayintervals[[x]]) <- days[[x]]
      return(tendayintervals[[x]])
    }))

  # get unique ten day-intervals for each data value in currenttrack
  timeinterval <- rep(0, length(currenttrack@data$time))
  days <- strftime(currenttrack@data$time, format = "%Y-%m-%d")
  iter <- 1
  for(day_i in unique(days)){
    timeinterval[which(days == day_i)] <- tendayintervals[which(names(tendayintervals) == day_i)]
    iter <- iter + 1
  }

  # adjust indices so that minimum is 1
  timeinterval <- timeinterval - min(timeinterval)+1

  return(timeinterval)

}

# function in order to classify on a daily resolution
assignday <- function(currenttrack){

  # get unique days
  timeinterval <- rep(0, length(currenttrack@data$time))
  days <- strftime(currenttrack@data$time, format = "%Y-%m-%d")
  iter <- 1
  for(day_i in unique(days)){
    timeinterval[which(days == day_i)] <- iter
    iter <- iter + 1
  }

  return(timeinterval)

}
