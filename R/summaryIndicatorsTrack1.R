#' Computes summary indicators for GPS tracks.
#'
#' \code{summaryIndicatorsTrack1} computes various summary indicators
#' for a \code{\link[trajectories:Track-class]{Track}} object that has been processed
#' with \code{\link{reorganizeTracks}}, \code{\link{locationsTrack}} or
#' \code{\link{clusterOrder}}, \code{\link{removeDataTrack}} and
#' \code{\link{nogapDurationTrack}}.
#'
#' @param currenttrack A \code{\link[trajectories:Track-class]{Track}} object that
#' has been processed as described in the description.
#' @param normalise A logical value indicating if the computed summary
#' indicators should be normalized relative to the duration of data values
#' (as specified by \code{currenttrack@data$nogap_duration}) for each time
#' interval (as specified by \code{currenttrack@data$id_timeinterval})
#' (\code{normalize = TRUE}) or not (\code{normalize = FALSE}).
#' @param timeinterval A numerical value reperesenting the duration of a
#' time interval represented by one data value of \code{currenttrack} [s].
#' @param aggregation A character value indicating for which spatial and
#' temporal aggregation of data values/visits at a location summary indicators
#' should be computed:
#' \describe{
#'   \item{\code{"raw"}}{Summary indicators will be computed on the (imputed)
#'   values of the original \code{\link[trajectories:Track-class]{Track}} object.}
#'   \item{\code{"allvisits"}}{Summary indicators will be computed on values
#'   aggregated for each visit, whereby it is distinguished between long-term
#'   visits (campsites) and short-term visits.}
#'   \item{\code{"campsites"}}{Like \code{"allvisits"}, but all short-term visits
#'   will be neglected during computations, i.e. only long-term visits will be
#'   considered.}
#' }
#' @return A \code{data.frame} object with with each row representing a time
#' interval as specified by \code{track@data$id_timeinterval} containing the
#' following variables:
#' \describe{
#'   \item{\code{distance_tot}}{A numeric value representing the (normalized)
#'   total distance covered during a time interval [m].}
#'   \item{\code{altitude_tot}}{A numeric value representing the (normalized)
#'   total altitudinal distance covered (i.e. each metre covered in a vertical
#'   distance) during a time interval [m].}
#'   \item{\code{altitude_distance}}{A numeric value representing the (normalized)
#'   total altitudinal distance covered during a time interval [m] (i.e. the
#'   altitudinal difference between the first point in the time interval and the
#'   last point).}
#'   \item{\code{campsites_time}}{A numeric value representing the (normlaized)
#'   total time spend within campsites during a time interval [s].}
#'   \item{\code{nocampsites_time}}{A numeric value representing the (normlaized)
#'   total time spend not in campsites during a time interval [s] (corresponds (?)
#'   to the travel time).}
#'   \item{\code{number_campsites}}{A numeric value representing the (normalized)
#'   total number of unique campsites (i.e. neglecting repeated visits or counting
#'   locations respectively) during a time interval.}
#'   \item{\code{number_repeatedvisits_campsite}}{A numeric value representing the
#'   (normalised) total number of repeated visits for all unique campsites during a
#'   time interval.}
#' }
#' @seealso \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
summaryIndicatorsTrack1 <- function(currenttrack,
                                    normalise = TRUE,
                                    timeinterval = 30*60,
                                    aggregation = "allvisits") {

  # collapse currenttrack to the specified aggregation level
  switch(aggregation,
         raw = {
           # no aggregation
         },
         allvisits = {
           # compute mean coordinates for each visit and location
           visitscoordinates <- extractCoordinatesVisitsTrack(currenttrack,
                                                              aggregated = FALSE)

           # aggregate all visits at all locations
           currenttrack$lon <- visitscoordinates$lon
           currenttrack$lat <- visitscoordinates$lat
           currenttrack$HEIGHT <- visitscoordinates$alt
         },
         campsites = {
           # compute mean coordinates for each visit and location
           visitscoordinates <- extractCoordinatesVisitsTrack(currenttrack,
                                                              aggregated = TRUE)

           # aggregate all visits at all locations
           currenttrack$lon <- visitscoordinates$lon
           currenttrack$lat <- visitscoordinates$lat
           currenttrack$HEIGHT <- visitscoordinates$alt
         }
         )

  # compute the summary indicators
  summaryindicators <- do.call(rbind, lapply(unique(currenttrack@data$id_timeinterval), function(timeinterval_i){

    # index for data values corresponding to timeinterval_i
    indextimeinterval <- which(currenttrack$id_timeinterval == timeinterval_i)

    # timeinterval_start, timeinterval_end
    timeinterval_start <- currenttrack$time[indextimeinterval[1]]
    timeinterval_end <- currenttrack$time[indextimeinterval[length(indextimeinterval)]]

    # duration_nogaps
    duration_nogaps <- currenttrack$duration_nogap[indextimeinterval[1]]

    if(length(which(currenttrack$gap[indextimeinterval] == FALSE)) > 1){

      # distance_tot
      distance_tot <- sum(currenttrack@connections$distance[indextimeinterval[-length(indextimeinterval)]][which(currenttrack$gap[indextimeinterval[-1]] == FALSE & currenttrack$gap[indextimeinterval[-length(indextimeinterval)]] == FALSE)]) * 1000

      # altitude_tot
      currenttrack$HEIGHT <- as.numeric(currenttrack$HEIGHT)
      altitude_tot <- sum(abs(currenttrack$HEIGHT[indextimeinterval[-1]] - currenttrack$HEIGHT[indextimeinterval[-length(indextimeinterval)]])[which(currenttrack$gap[indextimeinterval[-1]] == FALSE & currenttrack$gap[indextimeinterval[-length(indextimeinterval)]] == FALSE)]) - currenttrack$HEIGHT[indextimeinterval[1]]

      # altitude_distance
      indexnogap <- which(currenttrack$gap[indextimeinterval] == FALSE)
      altitude_distance <- abs(currenttrack$HEIGHT[indexnogap[length(indexnogap)]] - currenttrack$HEIGHT[indexnogap[1]])

      # campsites_time
      indexnogapcampsites <- which(currenttrack$gap[indextimeinterval[-length(indextimeinterval)]] == FALSE & currenttrack$campsite[indextimeinterval[-length(indextimeinterval)]] == TRUE)
      campsites_time <- sum(currenttrack@connections$duration[indextimeinterval[indexnogapcampsites]])

      # nocampsites_time
      indexnogapnocampsites <- which(currenttrack$gap[indextimeinterval[-length(indextimeinterval)]] == FALSE & currenttrack$campsite[indextimeinterval[-length(indextimeinterval)]] == FALSE)
      nocampsites_time <- sum(currenttrack@connections$duration[indextimeinterval[indexnogapnocampsites]])

      # number_campsites
      number_campsites <- length(unique(currenttrack$location[indextimeinterval[indexnogapcampsites]]))

      # number_repeatedvisits_campsite
      number_visits_campsite <- unlist(tapply(currenttrack$visitscampsite[indextimeinterval[indexnogapcampsites]], currenttrack$location[indextimeinterval[indexnogapcampsites]], unique))
      number_repeatedvisits_campsite <- length(which(number_visits_campsite > 1))

      # remove
      remove1 <- currenttrack$remove[indextimeinterval[1]]

      # collect all indices in a vector
      c(timeinterval_i, timeinterval_start, timeinterval_end, duration_nogaps,
        distance_tot, altitude_tot, altitude_distance, campsites_time,
        nocampsites_time, number_campsites, number_repeatedvisits_campsite, remove1)


    }else{

      c(timeinterval_i, timeinterval_start, timeinterval_end, duration_nogaps, NA, NA, NA, NA, NA, NA, NA, remove1)

    }

  }))

  # add variable names
  summaryindicators <- as.data.frame(summaryindicators, stringsAsFactors = FALSE)
  summaryindicators[,2] <- as.POSIXct(summaryindicators[,2], origin = "1970-01-01 00:00:00")
  summaryindicators[,3] <- as.POSIXct(summaryindicators[,3], origin = "1970-01-01 00:00:00")
  names(summaryindicators) <- c("timeinterval", "timeinterval_start", "timeinterval_end",
                                "duration_nogaps", "distance_tot", "altitude_tot", "altitude_distance",
                                "campsites_time", "nocampsites_time", "number_campsites",
                                "number_repeatedvisits_campsite", "remove")

  # normalise the values
  if(normalise == TRUE){

    summaryindicators[,5:11] <- do.call(rbind, lapply(seq_len(nrow(summaryindicators)), function(x){
      summaryindicators[x,5:11]/summaryindicators[x,4]
    }))

  }
}
