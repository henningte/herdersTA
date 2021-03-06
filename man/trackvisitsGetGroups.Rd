% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trackvisitsGetGroups.R
\name{trackvisitsGetGroups}
\alias{trackvisitsGetGroups}
\title{Groups Visits from Track Objects}
\usage{
trackvisitsGetGroups(
  trackvisits,
  tmin,
  timeinterval = 30 * 60,
  tmaxintersticenotvalid = NULL,
  tmaxinterstice
)
}
\arguments{
\item{trackvisits}{An object of class \code{\link{trackvisits}}.}

\item{tmin}{The minimum residence time at a specific location [s]
which is used to classify visits as campsites (if the residence
time at a specific location is larger than \code{tmin}) in
contrast to short-term visits of locations. The default is
\code{tmin = 345600}, i.e. 4 days.}

\item{timeinterval}{The number of seconds one data value (row) in
\code{currenttrack} covers.}

\item{tmaxintersticenotvalid}{A \code{data.frame} object that defines
time periods in which the argument \code{tmaxinterstice} will be ignored,
i.e. visits at the same location are merged irrespective of the duration
between these visits if there is no campsite visit at a different location
in-between. Each row indicates a time period in which this should be valid.
\code{tmaxintersticenotvalid} must contain two columns:
\describe{
  \item{\code{start}}{Represents the start time of the time interval.}
  \item{\code{end}}{Represents the end time of the time interval.}
}
It is evaluated for each visit if its endtime (\code{trackvisits$endtime})
is within any of the time periods or the starttime (\code{trackvisits$starttime})
of the next visit at the same location.}

\item{tmaxinterstice}{A numeric value giving the maximum duration of an
interstice between two visits at the same location until which
these visits are merged to one group if there is no campsite visit
in-between.}
}
\value{
An object of class \code{\link{trackvisits}}.
}
\description{
\code{trackvisitsGetGroups} defines groups of visits at the same location
based on (1) the duration between these visits, (2) the time of these visits
and (3) whether there is a campsite in-between. Additionally, the variabe
\code{mergewithnext} in the input \code{\link{trackvisits}} is defined, i.e.
it is given advice on visits to merge.
}
\examples{
#
}
\seealso{
\code{\link{trackvisits}},
\code{\link{plot.trackvisits}},
\code{\link{trackvisitsFromTrack}},
\code{\link{trackvisitsMergeGroups}},
\code{\link{trackvisitsSetNextvisitsamelocationindex}},
\code{\link{trackvisitsSetNorepeatedcampsitevisits}}.
}
