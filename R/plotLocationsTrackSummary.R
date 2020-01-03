#' @import ggplot2
#' @importFrom ggrepel geom_label_repel
NULL

#' Plots Summarised Tracks as Returned by \code{locationsTrack}.
#'
#' \code{plotLocationsTrackSummary} plots the result of \code{\link{locationsTrack}}
#' (with parameter  \code{summary = TRUE}) for a \code{\link[trajectories:Track-class]{Track}}
#' object. This means that idividual campsites are plotted as dots linked with a
#' line. Labels indicate the step (i.e. the sequence of visits), the location id,
#' the arrival time and the altitude of the location. The fill of the label box indicates
#' the season of the arrival time.
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
#' @seealso \code{\link{locationsTrack}}, \code{\link{locationsTracks}}.
#' @examples #
#' @export
plotLocationsTrackSummary <- function(x,
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

  # compute segments from the summary of the Track
  tracksegments <- data.frame(x = x$lon[-nrow(x)], y = x$lat[-nrow(x)], xend = x$lon[-1], yend = x$lat[-1])

  # define the labels
  labels <- paste0("step: ", seq_len(nrow(x)), ", ", "loc: ", x$location, ", ",
                   "vis.: ", x$visitscampsite, ",\n",
                   "arr.: ", strftime(x$arrivaltime, format = "%Y-%m-%d"), ", ",
                   "alt: ", round(x$alt, 0), " m, ",
                   "dep.: ", strftime(x$departuretime, format = "%Y-%m-%d"))

  # data.frame in order to plot segments for gaps
  plotdfsegmentsgaps <- data.frame(xstart = x$lon[-nrow(x)],
                                   xend = x$lon[-1],
                                   ystart = x$lat[-nrow(x)],
                                   yend = x$lat[-1])

  # retain only segments for gaps >= 24h + 20h (for at least one night, there were no values)
  plotdfsegmentsgaps <- plotdfsegmentsgaps[which(ifelse(difftime(as.POSIXct(x$departuretime[-nrow(x)]), as.POSIXct(x$arrivaltime[-1]), units = "sec") >= 44*60*60, TRUE, FALSE)),]

  # plot
  ggplot2::ggplot(data = x,
                  ggplot2::aes(x = lon,
                               y = lat)) +
    ggplot2::geom_point() +
    # arrow
    # ggplot2::geom_segment(data = tracksegments, ggplot2::aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.03, "npc"))) +
    ggplot2::geom_segment(data = tracksegments,
                          ggplot2::aes(x = x,
                                       y = y,
                                       xend = xend,
                                       yend = yend)) +
    ggplot2::geom_segment(data = plotdfsegmentsgaps,
                          ggplot2::aes(x = xstart,
                                       xend = xend,
                                       y = ystart,
                                       yend = yend),
                          colour = "white",
                          linetype = 2) +
    ggrepel::geom_label_repel(data = x,
                              ggplot2::aes(x = lon,
                                           y = lat,
                                           label = labels),
                              segment.colour = "gray",
                              point.padding = 0.2,
                              size = 1.5,
                              nudge_x = 0,
                              nudge_y = 0,
                              box.padding = 0.7,
                              fill = as.character(seasons$colour[seasonsarrival])) +
    ggplot2::coord_fixed() +
    ggplot2::theme(axis.text = element_blank()) +
    ggplot2::scale_x_continuous(limits = c(min(x$lon) - abs(diff(range(x$lon)))*0.15, max(x$lon) + abs(diff(range(x$lon)))*0.15)) +
    ggplot2::scale_y_continuous(limits = c(min(x$lat) - abs(diff(range(x$lat)))*0.15, max(x$lat) + abs(diff(range(x$lat)))*0.15))

}
