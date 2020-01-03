#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
NULL

#' Plots Altitude Profiles of Summarised Tracks as Returned by \code{\link{locationsTrack}}.
#'
#' \code{plotLocationsTrackSummaryAltitude} plots the result of \code{\link{locationsTrack}}
#' (with parameter  \code{summary = TRUE}) for a \code{\link[trajectories:Track-class]{Track}}
#' object. This means that idividual campsites are plotted as dots linked with a
#' line. Labels indicate the step (i.e. the sequence of visits), the location id,
#' the arrival time and the altitude of the location. The fill of the label box indicates
#' the season of the arrival time. In contratst to \code{\link{plotLocationsTrackSummary}},
#' this function plots the altitude profile over the time for a
#' \code{\link[trajectories:Track-class]{Track}} object.
#'
#' @param x An object as returned by \code{\link{locationsTrack}} with
#' parameter \code{summary = TRUE}.
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
#' @return A  \code{\link[ggplot2]{ggplot}} object.
#' @seealso \code{\link{locationsTrack}}, \code{\link{locationsTracks}},
#' \code{\link{plotLocationsTrackSummary}}.
#' @examples #
#' @export
plotLocationsTrackSummaryAltitude <- function(x,
                                              seasons = data.frame(start = c(3, 5, 9, 11),
                                                                   end = c(4, 8, 10, 2),
                                                                   colour = c("yellow", "red", "burlywood1", "lightgray"))){

  # get the season that crosses the end/begin of a year
  seasoncrossyear <- seasons$end[ifelse(seasons$start > seasons$end, TRUE, FALSE)]

  # adjust the value season that crosses the end/begin of a year
  seasons$end <- seasons$end + 12 * ifelse(seasons$start > seasons$end, 1, 0)

  # extract the month of the arrival time
  monthsarrival <- as.numeric(strftime(x$arrivaltime, format = "%m"))

  # adjust the values of monthsarrrival <= seasoncrossyear
  monthsarrival <- monthsarrival + 12 * ifelse(monthsarrival <= seasoncrossyear, 1, 0)

  # define for each arrival the corresponding season
  seasonsarrival <- sapply(monthsarrival, function(month){
    which(ifelse(month >= seasons$start & month <= seasons$end, TRUE, FALSE))
  })

  # define the labels
  labels <- paste0("step: ", seq_len(nrow(x)), ", ", "loc: ", x$location, ", ",
                   "vis.: ", x$visitscampsite, ",\n",
                   "arr.: ", strftime(x$arrivaltime, format = "%Y-%m-%d"), ", ",
                   "alt: ", round(x$alt, 0), " m, ",
                   "dep.: ", strftime(x$departuretime, format = "%Y-%m-%d"))

  # data.frame for plotting the points
  plotdfpoints <- data.frame(time = unlist(lapply(seq_len(nrow(x)), function(z) as.character(x[z,which(names(x) %in% c("arrivaltime", "departuretime"))]))), alt = rep(x$alt, each = 2), campsite = rep(x$campsite, each = 2), stringsAsFactors = FALSE)
  plotdfpoints$time <- as.POSIXct(as.numeric(plotdfpoints$time), origin = "1970-01-01 00:00:00")

  # data.frame in order to plot segments for gaps
  plotdfsegmentsgaps <- data.frame(xstart = as.POSIXct(x$departuretime[-nrow(x)]),
                                   xend = as.POSIXct(x$arrivaltime[-1]),
                                   ystart = x$alt[-nrow(x)],
                                   yend = x$alt[-1])

  # retain only segments for gaps >= 24h + 20h (for at least one night, there were no values)
  plotdfsegmentsgaps <- plotdfsegmentsgaps[which(ifelse(difftime(plotdfsegmentsgaps$xend, plotdfsegmentsgaps$xstart, units = "sec") >= 44*60*60, TRUE, FALSE)),]

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
  ggplot2::ggplot(data = plotdfpoints,
                  ggplot2::aes(x = time,
                               y = alt)) +
    ggplot2::geom_path() +
    ggplot2::geom_segment(data = plotdfsegmentsgaps,
                          ggplot2::aes(x = xstart,
                                       xend = xend,
                                       y = ystart,
                                       yend = yend),
                          colour = "white",
                          linetype = 2) +
    ggplot2::geom_point(ggplot2::aes(fill = plotdfpoints$campsite),
                        shape = 21,
                        size = 3) +
    ggplot2::scale_fill_manual(values = scale_fill_manual_values) +
    ggplot2::theme(legend.position = "none") +
    # arrow
    # geom_segment(data = tracksegments, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.03, "npc"))) +
    ggrepel::geom_label_repel(data = plotdfpoints[seq(1, nrow(plotdfpoints)-1, by = 2),],
                              ggplot2::aes(x = time,
                                           y = alt,
                                           label = labels),
                              segment.colour = "gray",
                              point.padding = 0.2,
                              size = 1.5,
                              nudge_x = 0,
                              nudge_y = 0,
                              box.padding = 0.7,
                              fill = as.character(seasons$colour[seasonsarrival]))

}
