#'@importFrom Rdpack reprompt
#'@import spatstat
#'@import sp
NULL

#' Determines the number of visits of a cluster.
#'
#' \code{searchNextVisit} checks whether the clusters/campsites have been
#' visited more than the currently known visits (\code{times_visited}).
#' Updates \code{times_visited} and determines the end (latest point in cell)
#' of the current visit. This function is used by function `
#' \code{\link{clusterOrder}}.
#'
#' @param clusters A \code{\link[sp]{SpatialGridDataFrame}} object containing
#' point clusters, as created with \code{\link{extractClusters}}.
#' @param trs_spdf A \code{\link[sp]{SpatialPointsDataFrame}} of the
#' household movement (as converted from a \code {\link[trajectories]{Tracks}}
#' object).
#' @return A \code{\link[sp]{SpatialPointsDataFrame}} holding the
#' clusters/campsites with updated \code{times_visited}.
#' @seealso \code{\link{qTopology}}, \code{\link{pointsPerQuad}},
#' \code{\link{extractClusters}}, \code{\link{findStarts}},
#' \code{\link{clusterOrder}}.
#' @examples #
#' @export
searchNextVisit <- function(clusters, trs_spdf){

  # get last occurence of points in cell from the cluster data frame
  last_in_cell <- clusters$last_in_cluster

  # get the start of current visit from the third last coloumn of data frame
  first_of_visit <- clusters@data[[ncol(clusters@data) - 2]]

  #get currently determined latest point of current visit, i.e. the second last coloumn
  last_of_visit <- clusters@data[[ncol(clusters@data)-1]]

  #get the current rank, i.e. the last coloumn
  current_rank <- clusters@data[[ncol(clusters@data)]]

  #loop through clusters by rank
  for(i in 1:(max(current_rank, na.rm = T) - 1)){

    # if current latest point is later in time than the earliest point of the following cluster...
    if(last_in_cell[which(current_rank == i)] > first_of_visit[which(current_rank == i + 1)]){

      # ...the cluster has been visited again later, so set times_visited + 1
      clusters$times_visited[which(current_rank == i)] <- clusters$times_visited[which(current_rank == i)] + 1

      # also determine end of current visit as the latest point in the cluster that is still earlier than the earliest point in the following cluster
      last_of_visit[which(current_rank == i)] <- max(na.omit(trs_spdf$time[trs_spdf$clrank == i & trs_spdf$time < first_of_visit[which(current_rank == i + 1)]]))

      # set newVisit to TRUE
      clusters$newVisit[which(current_rank == i)] <- TRUE

    }else{

      # set newVisit to FALSE
      clusters$newVisit[which(current_rank == i)] <- FALSE

    }

  }

  # the cluster which is last in line of 1st visits, always has only one visit
  clusters$newVisit[which(current_rank == max(current_rank, na.rm = T))] <- FALSE

  # update end time of current visit (second last coloumn) with newly determined last last_of_visit
  clusters@data[[ncol(clusters@data)-1]] <- last_of_visit

  # return clusters
  return(clusters)

}
