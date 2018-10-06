#' @importFrom Rdpack reprompt
#' @import trajectories
NULL

#' Extracts centroid coordinates of visits from GPS tracks.
#'
#' \code{centroidCoordinatesVisitsTrack} extract the mean coordinates for
#' visits at locations in a \code{\link[trajectories]{Track}} object as
#' returned by \code{\link{locationsTrack}}.
#'
#' Centroid coordinates can be computed for all visits at each location
#' (i.e. without aggregation of short-term visits with small temporal
#' interstices) (\code{aggregated = FALSE}) or for short-term visits and
#' long-term visits (cmapsites) (\code{aggregated = TRUE}) that may be
#' aggregated from visits at the same location if the interstice bewteen
#' the visits was smaller than \code{tmaxinterstices} as parametrised in
#' the previous \code{link{locationsTrack}} call.
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object as
#' returned by \code{link{locationsTrack}}
#' @param aggregated A logical value indicating if centroid coordinates
#' should be computed for all visits at each location
#' (i.e. without aggregation of short-term visits with small temporal
#' interstices) (\code{aggregated = FALSE}) or for short-term visits and
#' long-term visits (cmapsites) (\code{aggregated = TRUE}) that may be
#' aggregated from visits at the same location if the interstice bewteen
#' the visits was smaller than \code{tmaxinterstices} as parametrised in
#' the previous \code{link{locationsTrack}} call.
#' @return A \code{data.frame} object with centroid longitude values in
#' the first column, centroid latitude values in the second column and
#' centroid altitude values in the third column for each visit of a location
#' (as indicated by unique combinations of \code{currenttrack$location} and
#' \code{currenttrack$visitsloc}). Gaps will be omitted. The \code{data.frame}
#' object has the same number of rows as \code{currenttrack}.
#' @seealso \code{\link{identifyTimeIntervals}}, \code{\link{removeDataTrack}},
#' \code{\link{removeDataTracks}}, \code{\link{nogapDurationTracks}}.
#' @examples #
#' @export
extractCoordinatesVisitsTrack <- function(currenttrack, aggregated = FALSE){

  # convert currenttrack to a SpatialPointsDataFrame
  trsSP <- TrackToSpatialPointsDataFrame(currenttrack)

  # define an index for the location-visit combination
  indexvisitlocation <- paste0(trsSP$location, "_", trsSP$visitsloc)
  if(aggregated == TRUE){
    indexvisitlocation[na.omit(trsSP$campsite == TRUE)] <- paste0(trsSP$location[na.omit(trsSP$campsite == TRUE)], "_", trsSP$visitscampsite[na.omit(trsSP$campsite == TRUE)])
  }

  # compute mean values for each unique indexvisitlocation
  meanvalues <- data.frame(
    indexvisitlocation = unique(indexvisitlocation),
    lon = tapply(trsSP@coords[,1], indexvisitlocation, mean),
    lat = tapply(trsSP@coords[,2], indexvisitlocation, mean),
    alt = tapply(as.numeric(trsSP$HEIGHT), indexvisitlocation, mean)
    )

  data.frame(
    lon = do.call(c, lapply(unique(indexvisitlocation), function(x){rep(meanvalues[meanvalues[,1] == x, 2], length(which(indexvisitlocation == x)))})),
    lat = do.call(c, lapply(unique(indexvisitlocation), function(x){rep(meanvalues[meanvalues[,1] == x, 3], length(which(indexvisitlocation == x)))})),
    alt = do.call(c, lapply(unique(indexvisitlocation), function(x){rep(meanvalues[meanvalues[,1] == x, 4], length(which(indexvisitlocation == x)))}))
  )

}
