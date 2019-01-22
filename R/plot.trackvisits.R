#' @importFrom Rdpack reprompt
#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
NULL

#' Plots Visits from a Track with Locations
#'
#' \code{plot.trackvisits} is the plot function for objects of class
#' \code{\link{trackvisits}}. Several plots are possible:
#' \describe{
#'   \item{\code{"lonlat"}}{Plots the longitude on the x axis and the latitude
#'   on the y axis. Vists are plotted as points and are linked by lines. Points
#'   of visits at campsites are filled black, points of visits at short-term visits
#'   are filled white. Lines linking visits are plotted solid if the duration between
#'   visits does not exceed certain threshold values. Otherwise, they are plotted
#'   dashed. Each visit is indicated by a label that contains the following information:
#'   (1) step: The id of the visit, (2) loc.: The id of the location, (3) visit: The number
#'   of repeated campsite visits at the same location, (4) arr.: The start time of the
#'   visit (irrespective whether this is sure or not), (5) dep.: The end time of the
#'   visit (irrespective whether this is sure or not). Label boxes are filled according
#'   to an argument \code{"seasons}.}
#'   \item{\code{"lonlatanonymuous"}}{Like \code{"lonlat"}, but no labels will be
#'   added to the axes.}
#'   \item{\code{"altitude"}}{Plots the start time and end time of each visit on
#'   the x axis and the altitude of each visit on the y axis. Each visit is plotted
#'   as a pair of points linked by a solid straight line. Points
#'   of visits at campsites are filled black, points of visits at short-term visits
#'   are filled white. Lines linking visits are plotted solid if the duration between
#'   visits does not exceed certain threshold values. Otherwise, they are plotted
#'   dashed.Each visit is indicated by a label that contains the following information:
#'   (1) step: The id of the visit, (2) loc.: The id of the location, (3) visit: The number
#'   of repeated campsite visits at the same location, (4) arr.: The start time of the
#'   visit (irrespective whether this is sure or not), (5) dep.: The end time of the
#'   visit (irrespective whether this is sure or not). Label boxes are filled according
#'   to an argument \code{"seasons}.}
#' }
#'
#' @param x An object of class \code{link{trackvisits}} with the variable
#' \code{campsite} being defined.
#' @param what A character value defining which plot to create. See the description section.
#' @param seasons A \code{data.frame} object with a row for each season
#' and three columns:
#' \describe{
#'   \item{\code{start}}{A numeric value indicating the start month of the respective
#'   season.}
#'   \item{\code{start}}{A numeric value indicating the end month of the respective
#'   season.}
#'   \item{\code{colour}}{A character value indicating the colour used in order to fill
#'   the label boxes of visits with arrivals within the respective seasons.}
#' }
#' @param timethreshold A numeric value representing a time threshold of duarions between
#' adjacent visits [s]. If the duration between a visit and a next visit is larger than
#' \code{timethreshold}, this will be shown as dashed line in the plot.
#' @return A  \code{\link[ggplot2]{ggplotobj}} object
#'
#' @seealso \code{\link{reorganizeTracks}}, \code{\link{redefineIndices}},
#' \code{\link{fillGapTrack}}, \code{\link{fillGapTracks}},
#' \code{\link{extractClustersBuffer}}.
#' @examples #
#' @export
plot.trackvisits <- function(x, what = "lonlatanonymuous", seasons = data.frame(start = c(3, 5, 9, 11), end = c(4, 8, 10, 2), colour = c("yellow", "red", "burlywood1", "lightgray")), timethreshold = 0
){

  # checks
  if(!(inherits(x, "trackvisits"))){
    stop("x must be of class trackvisits\n")
  }
  if(!(what %in% c("lonlat", "lonlatanonymuous", "altitude"))){
    stop("what must be one of 'lonlat', 'lonlatanonymuous', 'altitude'\n")
  }

  # get the season that crosses the end/begin of a year
  seasoncrossyear <- seasons$end[ifelse(seasons$start > seasons$end, TRUE, FALSE)]

  # adjust the value season that crosses the end/begin of a year
  seasons$end <- seasons$end + 12 * ifelse(seasons$start > seasons$end, 1, 0)

  # extract the month of the arrival time
  monthsarrival <- as.numeric(strftime(x$starttime, format = "%m"))

  # adjust the values of monthsarrrival <= seasoncrossyear
  monthsarrival <- monthsarrival + 12 * ifelse(monthsarrival <= seasoncrossyear, 1, 0)

  # define for each arrival the corresponding season
  seasonsarrival <- sapply(monthsarrival, function(month){
    which(ifelse(month >= seasons$start & month <= seasons$end, TRUE, FALSE))
  })

  # data.frame for plotting the points
  plotdfpoints <- data.frame(x = rep(x$longitude, each = 2),
                             y = rep(x$latitude, each = 2),
                             time = unlist(lapply(seq_len(nrow(x)), function(z) as.character(x[z,which(names(x) %in% c("starttime", "endtime"))]))),
                             alt = rep(x$altitude, each = 2),
                             campsite = rep(x$campsite, each = 2),
                             stringsAsFactors = FALSE)
  plotdfpoints$time <- as.POSIXct(as.numeric(plotdfpoints$time), origin = "1970-01-01 00:00:00")

  # compute segments from the summary of the Track
  tracksegments <- data.frame(x = x$longitude[-nrow(x)],
                              y = x$latitude[-nrow(x)],
                              xend = x$longitude[-1],
                              yend = x$latitude[-1])

  # define the labels
  labels <- paste0("step: ", seq_len(nrow(x)), ", ", "loc: ", x$location, ", ",
                   "vis.: ", x$norepeatedcampsitevisits, ",\n",
                   "arr.: ", strftime(x$starttime, format = "%Y-%m-%d"), ", ",
                   "alt: ", round(x$altitude, 0), " m, ",
                   "dep.: ", strftime(x$endtime, format = "%Y-%m-%d"))

  # data.frame in order to plot segments for gaps
  plotdfsegmentsgaps <- data.frame(xstart = x$longitude[-nrow(x)],
                                   xend = x$longitude[-1],
                                   ystart = x$latitude[-nrow(x)],
                                   yend = x$latitude[-1],
                                   xstarttime = x$endtime[-nrow(x)],
                                   xendtime = x$starttime[-1],
                                   ystartaltitude = x$altitude[-nrow(x)],
                                   yendaltitude = x$altitude[-1])

  # retain only segments for gaps >= 24h + 20h (for at least one night, there were no values)
  plotdfsegmentsgaps <- plotdfsegmentsgaps[which(ifelse(abs(difftime(as.POSIXct(x$endtime[-nrow(x)]), as.POSIXct(x$starttime[-1]), units = "sec")) >= timethreshold, TRUE, FALSE)),]

  # define the scale_fill_manual_values
  if(length(unique(plotdfpoints$campsite)) == 2){
    scale_fill_manual_values <- c("white", "black")
  }else{
    if(unique(plotdfpoints$campsite) == TRUE){
      scale_fill_manual_values <- "black"
    }else{
      scale_fill_manual_values <- "white"
    }
  }

  # plot
  switch(what,
         lonlat = {

           ggplot() +
             geom_point(data = plotdfpoints, aes(x = x, y = y, fill = plotdfpoints$campsite), shape = 21, size = 3) +
             geom_segment(data = tracksegments, aes(x = x, y = y, xend = xend, yend = yend)) +
             geom_segment(data = plotdfsegmentsgaps, aes(x = xstart, xend = xend, y = ystart, yend = yend), colour = "white", linetype = 2) +
             geom_label_repel(data = plotdfpoints[which(seq_len(nrow(plotdfpoints)) %% 2 == 0)-1,], aes(x = x, y = y, label = labels), segment.colour = "gray", point.padding = 0.2, size = 1.5, nudge_x = 0, nudge_y = 0, box.padding = 0.7, fill = as.character(seasons$colour[seasonsarrival])) +
             coord_fixed() +
             theme(legend.position = "none") +
             scale_fill_manual(values = scale_fill_manual_values) +
             scale_x_continuous(limits = c(min(x$longitude) - abs(diff(range(x$longitude)))*0.15, max(x$longitude) + abs(diff(range(x$longitude)))*0.15)) +
             scale_y_continuous(limits = c(min(x$latitude) - abs(diff(range(x$latitude)))*0.15, max(x$latitude) + abs(diff(range(x$latitude)))*0.15))

         },
         lonlatanonymuous = {

           ggplot() +
             geom_point(data = plotdfpoints, aes(x = x, y = y, fill = plotdfpoints$campsite), shape = 21, size = 3) +
             geom_segment(data = tracksegments, aes(x = x, y = y, xend = xend, yend = yend)) +
             geom_segment(data = plotdfsegmentsgaps, aes(x = xstart, xend = xend, y = ystart, yend = yend), colour = "white", linetype = 2) +
             geom_label_repel(data = plotdfpoints[which(seq_len(nrow(plotdfpoints)) %% 2 == 0)-1,], aes(x = x, y = y, label = labels), segment.colour = "gray", point.padding = 0.2, size = 1.5, nudge_x = 0, nudge_y = 0, box.padding = 0.7, fill = as.character(seasons$colour[seasonsarrival])) +
             coord_fixed() +
             theme(axis.text = element_blank(), legend.position = "none") +
             scale_fill_manual(values = scale_fill_manual_values) +
             scale_x_continuous(limits = c(min(x$longitude) - abs(diff(range(x$longitude)))*0.15, max(x$longitude) + abs(diff(range(x$longitude)))*0.15)) +
             scale_y_continuous(limits = c(min(x$latitude) - abs(diff(range(x$latitude)))*0.15, max(x$latitude) + abs(diff(range(x$latitude)))*0.15))

         },
         altitude = {

           ggplot() +
             geom_path(data = plotdfpoints, aes(x = time, y = alt)) +
             geom_segment(data = plotdfsegmentsgaps, aes(x = xstarttime, xend = xendtime, y = ystartaltitude, yend = yendaltitude), colour = "white", linetype = 2) +
             geom_point(data = plotdfpoints, aes(x = time, y = alt, fill = plotdfpoints$campsite), shape = 21, size = 3) +
             scale_fill_manual(values = scale_fill_manual_values) +
             theme(legend.position = "none") +
             geom_label_repel(data = plotdfpoints[seq(1, nrow(plotdfpoints)-1, by = 2),], aes(x = time, y = alt, label = labels), segment.colour = "gray", point.padding = 0.2, size = 1.5, nudge_x = 0, nudge_y = 0, box.padding = 0.7, fill = as.character(seasons$colour[seasonsarrival]))

         },
         stop("Invalid what\n")
  )
}
