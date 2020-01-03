#' @importFrom stats na.omit
NULL
#' #' Extracts a Vector of Matched Unique Values for Fixed Ten-Day Intervals
#'
#' \code{summaryIndicatorsIntervalsTrack_categorylist} extracts for each
#' fixed ten-day interval a vector of unique values of a categorial variable,
#' sorted according to the temporal order of these values within each
#' fixed ten-day interval. The function requires a
#' \code{\link[trajectories:Track-class]{Track}} object as returned by
#' \code{\link{aggregateDailyLocationsTrack}}.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object that
#' has been processed as described in the description.
#' @param what A numeric value indicating a variable in the \code{data} slot of
#' \code{currenttrack} for which to extract the categories.
#' @param whatcondition An optional numeric value indicating a variable in the
#' \code{data} slot of \code{currenttrack} that should function as condition in order
#' to subset the values of the variable represented by \code{what}.
#' @param conditionvalue A value of the variable indicated by \code{whatcondition}.
#' Will only be considered if \code{!is.null(whatcondition)}. The values of the variable
#' represented by \code{what} will be filtered for \code{conditionvalue} in the variable
#' indicated by \code{whatcondition} prior summarising.
#' @return A vector of JSON representing the unique values of the selected categorial
#' variable occuring within each fixed ten-dy intervals.
#' @seealso \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
summaryIndicatorsIntervalsTrack_categorylist <- function(currenttrack,
                                                         what,
                                                         whatcondition = NULL,
                                                         conditionvalue = TRUE
){

  # check if currenttrack is of class Track
  if(!inherits(currenttrack, "Track")){
    stop("currenttrack must be of class Track\n")
  }

  # assign each day in currenttrack$day to a ftdi
  ftdi <- assignFixedTenDayInterval(as.POSIXct(currenttrack$day, format = "%Y-%m-%d"),
                                    startnew = FALSE)
  ftdi <- ftdi[names(ftdi) %in% currenttrack$day]
  ftdi <- ftdi - min(ftdi) + 1

  # summarise the values for each ftdi
  summarisedvalues <-
    as.data.frame(cbind(names(ftdi[!duplicated(ftdi)]), tapply(seq_along(ftdi), ftdi, function(x){

      # subset x if specified
      if(!is.null(whatcondition)){
        x <- x[currenttrack@data[x,whatcondition] == conditionvalue]
      }

      # extract the unique categorial values
      whatvaluesftdi <- stats::na.omit(currenttrack@data[x,what])
      whatvaluesftdi <- whatvaluesftdi[!duplicated(whatvaluesftdi)]
      if(length(whatvaluesftdi) > 0){
        names(whatvaluesftdi) <- paste0("V", seq_along(whatvaluesftdi))
      }

      # convert to JSON
      toJSON(whatvaluesftdi)

    })), stringsAsFactors = FALSE)
  names(summarisedvalues) <- c("ftdi", colnames(currenttrack@data)[what])

  # return summarisedvalues
  return(summarisedvalues)

}
