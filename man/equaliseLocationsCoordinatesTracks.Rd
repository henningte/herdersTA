% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/equaliseLocationsCoordinatesTracks.R
\name{equaliseLocationsCoordinatesTracks}
\alias{equaliseLocationsCoordinatesTracks}
\title{Equalise Track Coordinates to Locations.}
\usage{
equaliseLocationsCoordinatesTracks(currenttracks, cores = 1, clcall = NULL)
}
\arguments{
\item{currenttracks}{A \code{\link[trajectories:Track-class]{TracksCollection}}
object with one \code{\link[trajectories:Track-class]{Track}} object per
\code{\link[trajectories:Track-class]{Tracks}} object that has a variable
\code{location} and a variable \code{filled} in the data slot.
\code{location} must be numeric with an integer value for each unique location.
Gaps must have the location \code{0}.}

\item{cores}{An integer value representing the number of cores to
use in parallel computing.}

\item{clcall}{A function that is passed to
\code{\link[parallel:clusterApply]{clusterCall}}.}
}
\value{
\code{currenttracks} with the same longitude and latitude values for
each location for all \code{\link[trajectories:Track-class]{Track}} objetcs.
}
\description{
\code{equaliseLocationsCoordinatesTracks} computes the median
coordinates (longitude, latitude) for each location of all tracks
(object of class \code{\link[trajectories:Track-class]{Track}})
of an object of class \code{\link[trajectories:Track-class]{TracksCollection}}
and assigns these coordinates to matching data values in each track.
During this procedure, filled values are not considered in order to
not influence the median coordinates by the gap filling procedure.
The function uses \code{\link{equaliseLocationsCoordinatesTrack}}.
}
\examples{
#
}
\seealso{
\code{\link{locationsTrack}}.
}
