% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trackvisitsSetNorepeatedcampsitevisits.R
\name{trackvisitsSetNorepeatedcampsitevisits}
\alias{trackvisitsSetNorepeatedcampsitevisits}
\title{Get the Number of Repeated Campsite Visits.}
\usage{
trackvisitsSetNorepeatedcampsitevisits(currenttrackvisits)
}
\arguments{
\item{currenttrackvisits}{An object of class \code{\link{trackvisits}} for
which the variables \code{campsite} and \code{nextvisitsamelocation} are
defined.}
}
\value{
An object of class \code{\link{trackvisits}} in which the number of
repeated campsite visits at the same location is set.
}
\description{
\code{trackvisitsSetNorepeatedcampsitevisits} sets the number of repeated campsite visits
for each location of an object of class \code{\link{trackvisits}}. A visit is a repeated
campsite visit if (1) it is a campsite visit and (2) the previous visit is at a different
location.
}
\examples{
#
}
\seealso{
\code{\link{trackvisits}},
\code{\link{plot.trackvisits}},
\code{\link{trackvisitsFromTrack}},
\code{\link{trackvisitsGetGroups}},
\code{\link{trackvisitsMergeGroups}},
\code{\link{trackvisitsSetNextvisitsamelocationindex}}.
}
