% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractPolygonsTracks.R
\name{extractPolygonsTracks}
\alias{extractPolygonsTracks}
\title{Extracts corresponding raster values for GPS tracks.}
\usage{
extractPolygonsTracks(
  x,
  y,
  fn = NULL,
  ...,
  fixedlocationcoords = TRUE,
  what = NULL,
  cores,
  clcall = NULL
)
}
\arguments{
\item{x}{A \code{\link[sp:SpatialPolygons]{SpatialPolygons}} object.}

\item{y}{A \code{\link[trajectories:Track-class]{TracksCollection}} object.
Depending on other parameters set, there may be certain variables required.}

\item{fn}{function to summarize the values (e.g. \code{mean}).}

\item{...}{further arguments passed to \code{\link[sp:over]{over}}.}

\item{fixedlocationcoords}{A logical value indicating if for each location in \code{y}
the same position is assumed for all data values. In this case, computation
can be speed up by setting \code{location = TRUE} and the function uses
simply the coordinates of the first value for each location.}

\item{what}{A numeric vector specifying the column(s) of \code{x}
from which to retain values. If set to \code{NULL}, values for all
columns will be extracted.}

\item{cores}{An integer value representing the number of cores to
use in parallel computing.}

\item{clcall}{A function that is passed to
\code{\link[parallel:clusterApply]{clusterCall}}.}
}
\value{
A vector with a value for each data value of \code{y}.
If a value of \code{x} cannot be assigned to a respective data value,
\code{NA} is returned for the respective data value.
}
\description{
\code{extractPolygonsTracks} is a function
in order to extract values from \code{\link[sp:SpatialPolygons-class]{SpatialPolygons}} objects
for an object of class \code{\link[trajectories:Track-class]{TracksCollection}}.
}
\examples{
#
}
\seealso{
\code{\link{extractRasterTrack}},
\code{\link{extractPolygonsTrack}}.
}
