#' @importFrom Rdpack reprompt
#' @import spacetime
#' @import lubridate
#' @import trajectories
#' @import rgdal
#' @import sp
#' @import rgeos
NULL

#' Identifies and classifies visits in GPS tracks.
#'
#' \code{locationsTrack} identifies clusters (locatons) of
#' points in GPS tracks (\code{\link[trajectories]{Track}}
#' object) based on their spatial proximity using
#' \code{\link{extractClustersBuffer}} and identifies individual
#' visits of the same cluster (location) along the track.
#' Additionally, visits are classified as long-term visits
#' (campsites) or short-term visits.
#'
#' The function can be used in order to assign to each data
#' value of the input (\code{\link[trajectories]{Track}}
#' object an id of the cluster it is assigned to (
#' \code{summary = FALSE}) or to summarise the information
#' for each visit of a location (\code{summary = TRUE})
#'
#' @param currenttrack A \code{\link[trajectories]{Track}} object.
#' @param radius A numerical value representing the radius of the
#' buffers computed around each point [m] which are used for
#' clustering values to locations. Default is \code{radius = 800} [m].
#' @param tmin The minimum residence time at a specific location [s]
#' which is used to classify visits as campsites (if the residence
#' time at a specific location is larger than \code{tmin}) in
#' contrast to short-term visits of locations. The default is
#' \code{tmin = 345600}, i.e. 4 days.
#' @param summary Logical value indicating if the information on the
#' locations and visits should be summarised (\code{summary = TRUE})
#' or not (\code{summary = FALSE}). See the details section for further
#' information.
#' @return A \code{\link[trajectories]{Track}} object that is:
#' \describe{
#'   \item{If (\code{summary = TRUE})}{identical to the input
#'   \code{\link[trajectories]{Track}} object, but has four additional
#'   columns in the \code{data} slot:
#'   \describe{
#'     \item {\code{location}}{An integer value for each identified
#'     spatial point cluster (location) increasing with the time starting
#'     from 1.}
#'     \item{\code{campsite}}{A logical value indicating if a visits of a
#'     location is classified as long-term visit (campsite) (\code{TRUE})
#'     or as short-term visit (\code{FALSE}).}
#'     \item{\code{visitsloc}}{An integer vector indicating the number of
#'     visits at a specific location the data point is assigned to (i.e.
#'     a counter for the visits at a specific location).}
#'     \item{\code{visitscampsite}}{An integer vector indicating the number
#'     of visits at a specific location the data point is assigned to that
#'     are classified as long-term visits (campsites) (i.e.
#'     a counter for the long-term visits at a specific location).}
#'   }
#'   Gaps, as indicated by the column \code{gap}, have \code{NA} values for
#'   all four variables.
#'   }
#'   \item{If (\code{summary = FALSE})}{A \code{\link[trajectories]{Track}}
#'   object summarising the locations and visits of the input
#'   \code{\link[trajectories]{Track}} object with the following variables:
#'   \describe{
#'     \item{location}{An integer value for each identified
#'     spatial point cluster (location) increasing with the time starting
#'     from 1.}
#'     \item{residencetime}{A numerical vector indicating the residence time
#'     of each visit [s].}
#'     \item{cmapsite}{A logical value indicating if a visits of a
#'     location is classified as long-term visit (campsite) (\code{TRUE})
#'     or as short-term visit (\code{FALSE}).}
#'     \item{arrivaltime}{A \code{POSIXct} vector indicating the arrival time
#'     of the visit at the respective location.}
#'     \item{departuretime}{A \code{POSIXct} vector indicating the departure
#'     time of the visit from the respective location.}
#'     \item{arrivaltimeindex}{An integer vector indicating the index of the
#'     data value of the input \code{\link[trajectories]{Track}} object that
#'     represents the arrival at the location for the respective visit.}
#'     \item{departuretimeindex}{An integer vector indicating the index of the
#'     data value of the input \code{\link[trajectories]{Track}} object that
#'     represents the departure from the location for the respective visit.}
#'     \item{\code{visitsloc}}{An integer vector indicating the number of
#'     visits at a specific location the data point is assigned to (i.e.
#'     a counter for the visits at a specific location).}
#'     \item{\code{visitscampsite}}{An integer vector indicating the number
#'     of visits at a specific location the data point is assigned to that
#'     are classified as long-term visits (campsites) (i.e.
#'     a counter for the long-term visits at a specific location).}
#'     \code{lon}{The longitude of the respective location (as mean value of
#'     the coordinates of the data values assigned to the visit).}
#'     \code{lat}{The latitude of the respective location (as mean value of
#'     the coordinates of the data values assigned to the visit).}
#'     \code{alt}{The altitude of the respective location (as mean value of
#'     the coordinates of the data values assigned to the visit).}
#'     \code{speed}{The speed of the respective location (as mean value of
#'     the speed values of the data values assigned to the visit).}
#'   }
#'   }
#' }
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{redefineIndices}},
#' \code{\link{fillGapTrack}}, \code{\link{fillGapTracks}},
#' \code{\link{extractClustersBuffer}}.
#' @examples #
#' @export
locationsTracks <- function(currenttrack,
                            radius = 200,
                            tmin = 345600,
                            summary = TRUE
){

  # convert track to SpatialPointsDataFrame and transform to UTM
  trsSP <- TrackToSpatialPointsDataFrame(currenttrack)

  # cluster the data points
  initiallocationids <- extractClustersBuffer(trsSP, radius)

  # add variable to trsSP
  trsSP$campsites <- initiallocationids

  # return NULL if a data set contains only one location (no Track object can be constructed from one point)
  if(length(unique(initiallocationids)) == 1){
    warning("Only one location identified!")
    return(NULL)
  }

  # find order of campsite visits
  ordercampsitevisits <- lapply(unique(initiallocationids)[unique(initiallocationids) != 0], function(x){
    identifyBlocksVariable(track = trsSP, variable = "campsites", value = x)}
  )
  ordercampsitevisits <- as.matrix(do.call(rbind, ordercampsitevisits))
  ordercampsitevisits <- ordercampsitevisits[order(ordercampsitevisits[,1]),]

  # create a list with respective indices
  ordercampsitevisits <- lapply(seq_len(nrow(ordercampsitevisits)), function(x){ordercampsitevisits[x,1]:(ordercampsitevisits[x,2])})

  # define a dataframe to store the results in (with the location id of each visit)
  campsitesvar <- data.frame(location = trsSP$campsites[sapply(ordercampsitevisits, function(x){x[1]})])

  # duration of each visit
  campsitesvar$residencetime = sapply(ordercampsitevisits, function(x){difftime(trsSP$time[range(x)[2]], trsSP$time[range(x)[1]], units = "sec")})

  # classify visits as campsites or short term visits
  campsitesvar$campsite <- rep(T, nrow(campsitesvar))
  campsitesvar$campsite[which(campsitesvar$residencetime < tmin)] <- FALSE

  # arrival and departure time
  campsitesvar$arrivaltime <- sapply(ordercampsitevisits, function(x){as.character(trsSP$time[x[1]])})
  campsitesvar$departuretime <- sapply(ordercampsitevisits, function(x){as.character(trsSP$time[x[length(x)]])})

  # arrival and departure time as row indices
  campsitesvar$arrivaltimeindex <- sapply(ordercampsitevisits, function(x){range(x)[1]})
  campsitesvar$departuretimeindex <- sapply(ordercampsitevisits, function(x){range(x)[2]})

  # number of visits per location
  visits <- rep(NA, nrow(campsitesvar))
  for(loc_i in unique(campsitesvar$location)){

    visits[which(campsitesvar$location == loc_i)] <- seq(from = 1, to = length(which(campsitesvar$location == loc_i)))

  }
  campsitesvar$visitsloc <- visits

  # number of visits per campsite
  visits <- rep(NA, nrow(campsitesvar))
  for(loc_i in unique(campsitesvar$location)){

    visits[which(campsitesvar$location == loc_i & campsitesvar$campsite == T)] <- seq(from = 1, to = length(which(campsitesvar$location == loc_i & campsitesvar$campsite == TRUE)))

  }
  campsitesvar$visitscampsite <- visits

  # redefine indices of locations (according to arrival time)
  campsitesvar <- redefineIndices(df = campsitesvar, indices = "location", time = "arrivaltime", notchange = 0)

  # get mean position data for each location visit
  centr.coords <- data.frame()
  for(visit_i in c(1:length(ordercampsitevisits))){

    centr.coords <- rbind(centr.coords, apply(matrix(trsSP@coords[ordercampsitevisits[[visit_i]],], ncol = 2), 2 , mean))

  }
  centr.coords$alt <- sapply(ordercampsitevisits, function(x){mean(trsSP$HEIGHT[x])})
  names(centr.coords) <- c("lon", "lat", "alt")

  # convert to SP
  centr.coords <- SpatialPoints(centr.coords[,-3], proj4string = CRS(proj4string(trsSP)))

  # transform back to WGS84 longitude latitude
  a <- spTransform(centr.coords, currenttrack@sp@proj4string)

  # add variables to campsitesvar
  campsitesvar$lon <- a@coords[,1]
  campsitesvar$lat <- a@coords[,2]
  campsitesvar$alt <- sapply(ordercampsitevisits, function(x){mean(trsSP$HEIGHT[x])})

  # get mean speed at each location visit
  campsitesvar$speed <- sapply(ordercampsitevisits, function(x){mean(trsSP$SPEED[x])})

  # create SpatialPointsDataFrame from campsitesvar
  campsitesvar1 <- campsitesvar
  campsitesvar <- SpatialPoints(coords = data.frame(lon = campsitesvar$lon, lat = campsitesvar$lat), proj4string = CRS(
    "+proj=longlat +zone=46 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  ))
  campsitesvar <-
    spTransform(
      campsitesvar,
      CRS(
        "+proj=utm +zone=46 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
      )
    )

  # create Track object from campsitesvar
  campsitesvar <- Track(STIDF(sp = campsitesvar, time = as.POSIXct(campsitesvar1$arrivaltime) , data = campsitesvar1, endTime = as.POSIXct(campsitesvar1$departuretime)))

  # add information to currenttrack
  locationid <- rep(NA, nrow(trsSP@data))
  campsite <- rep(NA, nrow(trsSP@data))
  visitsloc <- rep(NA, nrow(trsSP@data))
  visitscampsite <- rep(NA, nrow(trsSP@data))
  for(visit_i in c(1:nrow(campsitesvar))){

    # get time interval of visit
    timeint <- c(campsitesvar$arrivaltimeindex[visit_i], campsitesvar$departuretimeindex[visit_i])

    # define condition
    cond <- seq(timeint[1], timeint[2])

    # add location id to locationid
    locationid[cond] <- campsitesvar$location[visit_i]

    # add information on campsite to campsite
    campsite[cond] <- campsitesvar$campsite[visit_i]

    # add information on number of visits to visitsloc
    visitsloc[cond] <- campsitesvar$visitsloc[visit_i]

    # add information on number of visits to visitscampsite
    visitscampsite[cond] <- campsitesvar$visitscampsite[visit_i]
  }

  # add locationid, campsite and visit to currenttrack.df
  currenttrack@data$location <- locationid
  currenttrack@data$campsite <- campsite
  currenttrack@data$visitsloc <- visitsloc
  currenttrack@data$visitscampsite <- visitscampsite

  # return the result
  ifelse(summary == TRUE, campsitesvar, currenttrack){

}
