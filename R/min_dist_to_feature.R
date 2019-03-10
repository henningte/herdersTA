
#'@importFrom Rdpack reprompt
#'@import rgdal
#'@import geosphere
#'@import sp
NULL

#' Computes the minimum distance of locations to the nearest line or polygon border
#'
#' \code{min_dist_to_feature} computes the shortest distance of each location (as identified by \code{\link{locationsTracks}}) in a \code{Tracks} object to the closest polyline or polygon border. It does not include all possible features, but starts looking for features in a certain distance from each location. If no features are found, the search distance is increased iteratively.
#' @param hh A \code{Tracks} object usually representing movement of one household.
#' @param feature A SpatialLines or SpatialPolygons object containing the features to compute distance to.
#' @return A \code{vector} with the shortest distance of all locations in \code{hh} to the closest feature in \code{feature}.
#' @seealso
#' @examples #
#' @export
min_dist_to_feature <- function(hh, feature, minrange=10000){

  #extract SpatialPoints of locations
  #locationIDs <- sort(unique(hh@tracks[[1]]@data$location[hh@tracks[[1]]@data$location>0]))
  #locationcoords <- unique(hh@tracks[[1]]@sp@coords[which(hh@tracks[[1]]@data$location %in% locationIDs),])
  #locations <- SpatialPoints(locationcoords, proj4string = hh@tracks[[1]]@sp@proj4string)

  locations <- SpatialPoints(unique(hh@tracks[[1]]@sp@coords), proj4string = hh@tracks[[1]]@sp@proj4string)

  #check CRS of both inputs
  if(!proj4string(locations)=="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
    locations <- spTransform(locations,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  }
  if(!proj4string(feature)=="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
    feature <- spTransform(feature,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  }

  distances <- c()
  #iterate through locations, for each location...
  for(i in 1:length(locations)){
    #check whether there are features in the minimum range (specified by minrange)
    bufferrange <- minrange
    feat_in_range <- suppressWarnings(intersect(feature,buffer(locations[i], bufferrange)))
    # if there are none, feat_in_range will be NULL, in this case extend range iteratively by 10000m until
    #feat_in_range is not NULL
    while(is.null(feat_in_range)){
      bufferrange <- bufferrange+10000
      feat_in_range <- suppressWarnings(intersect(feature,buffer(locations[i], bufferrange)))
    }
    #then calculate shortest distance to features in feat_in_range and add it to the vector of distances for
    #all locations
    distances[i] <- dist2Line(locations[i],feat_in_range)[,1]
  }

  return(distances)
}
