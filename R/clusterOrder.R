#' @importFrom sp over spTransform
NULL

#' Identifies clusters in GPS tracks considered as campsites.
#'
#' \code{clusterOrder} identifies campsites (clusters of points) of a household
#' (\code{\link[trajectories:Track-class]{Tracks}} object) and for each cluster/campsite
#' determines the number of visits, the order in which the clusters have been
#' visited, and the start and end times (first and last point) of each visit.
#'
#' \code{clusterOrder} uses the function \code{\link{extractClusters}} to
#' detect the campsites. \code{clusterOrder} also uses the functions
#' \code{link{searchNextVisit}} and \code{\link{findStarts}}.
#'
#' @param currenttracks A \code{\link[trajectories:Track-class]{Tracks}} object.
#' @param cellsize Numerical value representing the target size of the
#' quadrats to create.
#' @param threshold An integer value specifying the minimum number of
#' points within a grid cell needed in order to consider these points
#' as cluster.
#' @return A \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}} holding grid cells
#' determined as clusters including data on the number of visits per cluster,
#' start and end times etc.
#' @seealso \code{\link{qTopology}}, \code{\link{pointsPerQuad}},
#' \code{\link{extractClusters}}, \code{\link{searchNextVisit}},
#' \code{\link{findStarts}}.
#' @examples #
#' @export
clusterOrder <- function(currenttracks,
                         cellsize = 800,
                         threshold = 20){

  # extract the clusters within the grid
  clusters <- extractClusters(currenttracks = currenttracks,
                              targetQsize = cellsize,
                              threshold = threshold)

  # save id of cell in dataframe as "clid"
  clusters$clid <- rownames(clusters@data)

  # convert to SpatialPointsDataFrame
  trs_spdf <- sp::spTransform(as(currenttracks, "SpatialPointsDataFrame"),
                              CRSobj = clusters@proj4string)

  # assign clid of corresponding cluster to each point
  trs_spdf@data$clid <- sp::over(trs_spdf, clusters)$clid

  ## begin ordering
  # detect and rank first visit of each cluster/campsite:

  # determine first visit for each cluster
  visit1_start <- aggregate(trs_spdf["time"],
                            by = clusters,
                            FUN = min)$time

  # rank cluster regarding first visit
  visit1_rank <- rank(visit1_start)

  # determine latest point of the track within each cluster
  clusters$last_in_cluster <- aggregate(trs_spdf["time"],
                                        by = clusters,
                                        FUN = max)$time

  # temporarily set end of first visit to the last point in cluster (will be updated during ordering below)
  visit1_end <- clusters$last_in_cluster

  # create a variable rperesenting the number of visits of a cluster
  clusters$times_visited <- 1

  # add start, end and rank of visit to dataframe of the clusters/campsites
  clusters@data <- cbind(clusters@data, visit1_start, visit1_end, visit1_rank)
  remove(visit1_rank, visit1_start, visit1_end)

  # assign rank of cluster to corresponding points of trs_spdf
  trs_spdf@data$clrank <- sp::over(trs_spdf, clusters)$visit1_rank

  # create vector for holding information on whether a new visit has been detected
  newVisit <- rep(TRUE, length(clusters$visit1_rank))

  # attach information on new visits to the first coloumn in order to keep the last coloumn for the end of the corresponding visit
  clusters@data <- cbind(newVisit,clusters@data)

  # find furhter visits:

  #index for naming the coloumns
  i = 2

  # while there were at least two clusters with additional visits to be ordered in the previous iteration, continue. Otherwised ordering is finished
  while(length(which(clusters$newVisit == TRUE)) > 1){

    # in each cluster look for visits later than the currently known ones
    clusters <- searchNextVisit(clusters, trs_spdf)

    # if additional visits were found, find starts and ends of these visits, whereas ends are defined as latest points in each cluster (in the next iteration it will be checked,whether they belong to another visit of the same cluster)
    if(any(clusters$newVisit == TRUE)){

      clusters@data <-
        cbind(
          clusters@data,
          findStarts(clusters, trs_spdf),
          aggregate(trs_spdf["time"], by = clusters, FUN = max)$time
        )

      # set rows without additional visits to NA
      clusters@data[[ncol(clusters@data)]][is.na(clusters@data[[ncol(clusters@data)-1]])] <-NA

      # rank clusters according to start of current visit and add rank to data frame
      rank <- rank(clusters@data[[ncol(clusters@data)]])
      rank[which(is.na(clusters@data[[ncol(clusters@data)]]))] <- NA
      clusters@data <- cbind(clusters@data, rank)

      # rename coloumns
      names(clusters@data)[ncol(clusters@data)] <- paste("visit",i,"_rank",sep="")
      names(clusters@data)[ncol(clusters@data)-1] <- paste("visit",i,"_end",sep="")
      names(clusters@data)[ncol(clusters@data)-2] <- paste("visit",i,"_start",sep="")
      i <- i+1

    }

  }

  # print message
  print("Finished")

  # plot the clusters and connect them with lines
  plot(trs_spdf@data$time, trs_spdf@data$clrank, type = "l")

  # return clusters
  return(clusters)

}
