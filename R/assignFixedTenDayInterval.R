#' @importFrom Rdpack reprompt
#' @import trajectories
#' @import raster
NULL

#' Assigns days to fixed ten-day intervals.
#'
#' \code{assignFixedTenDayInterval} assigns each value of a \code{POSIXct}
#' vector to a specific fixed ten-day interval of a year (three intervals
#' per month from day one to 10, 11 to 20 and 21 to 28/29/30/31 for each
#' month, depending on the month and year. Leap years will be considered).
#' This is done by constructing an integer vector containing values for
#' each value of the \code{POSIXct} vector and unique values for each ten-
#' day interval.
#'
#' It can be specified if the ID for the ten-day intervals should be reset
#' for each new year (\code{startnew = TRUE}) or not (\code{startnew = FALSE}).
#'
#' @param timedate A \code{POSIXct} vector containing date (and time)
#' information.
#' @param startnew A logical value indicating if the ID for ten-day intervals
#' should be reset to \code{1} for each new year (\code{startbew = TRUE}) or
#' not (\code{startnew = FALSE}).
#' @return An integer vector with the same length as \code{timedate} assigning
#' to each value of \code{timedate} an ID of a fixed ten-day interval.
#' @seealso \code{\link{extractRasterTrack}},
#' \code{\link{extractRasterTracks}}.
#' @examples #
#' @export
assignFixedTenDayInterval <- function(timedate, startnew = TRUE){

  # get the years covered
  years <- unique(strftime(timedate, format = "%Y"))

  # get the number of days in each year
  days <- list()
  for(year_i in seq_along(years)){
    days[[year_i]] <- seq(as.POSIXct(paste0(years[year_i], "-01-01"), tz = attr(timedate, "tzone")), as.POSIXct(paste0(years[year_i], "-12-31"), tz = attr(timedate, "tzone")), "day")
  }
  days <- lapply(days, function(x) strftime(x, "%Y-%m-%d"))

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

  # get unique ten day-intervals for each data value in track
  timeinterval <- rep(0, length(timedate))
  days <- strftime(timedate, format = "%Y-%m-%d")
  iter <- 1
  iter2 <- 0
  for(day_i in unique(days)){
    timeinterval[which(days == day_i)] <- tendayintervals[which(names(tendayintervals) == day_i)] - iter2*36

    # reset iter after one year
    if((tendayintervals[which(names(tendayintervals) == day_i)] %% 36 == 0) && strftime(day_i, format = "%m-%d") == "12-31"){
      iter <- 1
      iter2 <- iter2 + 1
    }
  }

  if(startnew == TRUE){
    timeinterval
  }else{
    tendayintervals
  }

}

