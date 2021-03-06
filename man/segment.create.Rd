% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segment.R
\name{segment.create}
\alias{segment.create}
\title{Create a "segment" object}
\usage{
segment.create(states, dwells, seg = 1, start_time = 0,
  name = "burst", ignore_errors = FALSE)
}
\arguments{
\item{states}{a vector of states}

\item{dwells}{a vector of dwell durations (same length as states)}

\item{seg}{The segment number. Defaults to 1}

\item{start_time}{When the dwells began. Defaults to 0}

\item{name}{Suffix-less version of the original filename. 60uM.dwt -> '60uM'}

\item{ignore_errors}{Do not report faulty segments (not many reasons to do this)}
}
\value{
The segment object: A dataframe with extra metadata.
}
\description{
This is a low-level function, mostly for use internally by other functions. There aren't many reasons to use this. Create object containing table data and metadata. The object can be used as a dataframe, and the metadata can be accessed with the functions: segment.seg, segment.start_time, segment.filename
}
\examples{

# It's more likely that you created states or dwells with some function
states  <-  c(0,      1,    0,    1,    0,    1,    0,    1,    0,    1)
dwells  <-  c(0.1,  1.1,  0.5,  0.2,  1.0,  1.1,  0.6,  1.1,  0.8,  1.1)
my_burst <- segment.create(states, dwells, seg=1, start_time=0, name="example_segment")

segment.name(my_burst)

}
