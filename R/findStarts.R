#'@importFrom Rdpack reprompt
#'@import spatstat
#'@import sp
NULL

#' Determines the start times of visits of a cluster.
#'
#' \code{findStarts} determines the start date/time of the latest visit
#' in a cluster/campsite if an additional visit has been detected by
#' \code{\link{searchNextVisit}}.
#'
#' \code{\link{clusters}} should be an object of clusters/campsites as
#' returned by \code{\link{searchNextVisit}}. This function is used by
#' function \code{\link{clusterOrder}}.
#'
#' @param clusters A \code{\link[sp]{SpatialGridDataFrame}} object containing
#' point clusters, as created with \code{\link{extractClusters}}.
#' @param trs_spdf A \code{\link[sp]{SpatialPointsDataFrame}} of the
#' household movement (as converted from a \code {\link[trajectories]{Tracks}}
#' object).
#' @return A \code{POSIXct} vector of containing the starts (first points)
#' of the additional visits.
#' @seealso \code{\link{qTopology}}, \code{\link{pointsPerQuad}},
#' \code{\link{extractClusters}}, \code{\link{searchNextVisit}},
#' \code{\link{clusterOrder}}.
#' @examples #
#' @export
findStarts <- function(clusters, trs_spdf){

  # get the end of previous visit from the second last coloumn of data frame
  last_of_previous <- clusters@data[[ncol(clusters@data)-1]]

  # create a POSIXct vector for next start values (using visit1_start as dummy values)
  next_start <- clusters$visit1_start

  # get the current rank, i.e. the last coloumn
  current_rank <- clusters@data[[ncol(clusters@data)]]

  # loop through clusters following rank
  for(i in 1:(max(current_rank, na.rm = T))){

    # if new visit has been detected before:
    if(clusters$newVisit[which(current_rank == i)]  == TRUE){

      # determine next_start within cluster as the earliest point that is in that cluster and later than the end of the previous visit
      next_start[which(current_rank == i)] <- min(na.omit(trs_spdf$time[trs_spdf$clrank == i & trs_spdf$time > last_of_previous[which(current_rank == i)]]))

    }else{

      next_start[which(current_rank == i)] <- NA

    }

  }

  # return next_start
  return(next_start)

}
