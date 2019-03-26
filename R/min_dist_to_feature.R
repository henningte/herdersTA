#'@importFrom Rdpack reprompt
#'@import rgdal
#'@import geosphere
#'@import sp
#'#'@import raster
NULL

#' Computes the minimum distance of locations to the nearest line or polygon border
#'
#' \code{min_dist_to_feature} computes the shortest distance of each location
#' (as identified by \code{\link{locationsTracks}}) in a \code{Tracks} object
#' to the closest polyline or polygon border. It does not include all possible
#' features, but starts looking for features in a certain distance from each
#' location. If no features are found, the search distance is increased
#' iteratively.
#' @param currenttrack A \code{Tracs} object usually representing movement of one household.
#' The \code{data} slot of \code{currenttrack} must contain a variable \code{"location"}
#' representing unique IDs for locations. Values with the location \code{0} receive
#' distance values of \code{NA}.
#' @param feature A SpatialLines or SpatialPolygons object containing the features
#' to compute distance to.
#' @return A \code{vector} with the shortest distance of all locations in \code{currenttrack}
#' to the closest feature in \code{feature}. For each value in \code{currenttrack}, a distance
#' value will be returned in the order of data values.
#' @seealso
#' @examples #
#' @export
min_dist_to_feature <- function(currenttrack, feature, minrange = 10000){

  #extract SpatialPoints of locations
  #locationIDs <- sort(unique(currenttracks@tracks[[1]]@data$location[currenttracks@tracks[[1]]@data$location>0]))
  #locationcoords <- unique(currenttracks@tracks[[1]]@sp@coords[which(currenttracks@tracks[[1]]@data$location %in% locationIDs),])
  #locations <- SpatialPoints(locationcoords, proj4string = currenttracks@tracks[[1]]@sp@proj4string)

  # get index for values where a location occurs for the first time
  index <- which(!duplicated(currenttrack$location) & currenttrack$location != 0)
  distance <- data.frame(location = currenttrack$location[index], stringsAsFactors = FALSE)

  # extract locations as SpatialPoints
  locations <- SpatialPoints(currenttrack@sp@coords[index,, drop = FALSE], proj4string = currenttrack@sp@proj4string)

  #check CRS of both inputs
  if(!proj4string(locations)=="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
    locations <- spTransform(locations,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  }
  if(!proj4string(feature)=="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
    feature <- spTransform(feature,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  }

  #iterate through locations, for each location...
  distance$res <-
    sapply(seq_along(locations), function(i){
      #check whether there are features in the minimum range (specified by minrange)
      bufferrange <- minrange
      feat_in_range <- suppressWarnings(intersect(feature, buffer(locations[i], bufferrange)))
      # if there are none, feat_in_range will be NULL, in this case extend range iteratively by 10000m until
      #feat_in_range is not NULL
      while(length(feat_in_range) < 2){
        bufferrange <- bufferrange + 10000
        feat_in_range <- suppressWarnings(intersect(feature,buffer(locations[i], bufferrange)))
      }
      #then calculate shortest distance to features in feat_in_range and add it to the vector of distances for
      #all locations
      dist2Line(locations[i],feat_in_range)[,1]
    })

  # join the distance values to the data values in currenttrack@data
  dplyr::left_join(currenttrack@data, distance, by = "location")["res"]

}
