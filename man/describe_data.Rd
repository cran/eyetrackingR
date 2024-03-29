% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep.R
\name{describe_data}
\alias{describe_data}
\title{Describe dataset}
\usage{
describe_data(
  data,
  describe_column,
  group_columns,
  quantiles = c(0.025, 0.975)
)
}
\arguments{
\item{data}{Data already run through \code{make_eyetrackingr_data}}

\item{describe_column}{The column to return descriptive statistics about.}

\item{group_columns}{Any columns to group by when calculating descriptive statistics (e.g., participants,
conditions, etc.)}

\item{quantiles}{Numeric vector of length two with quantiles to compute (default: \code{c(.025, .975)}).}
}
\value{
A dataframe giving descriptive statistics for the \code{describe_column}, including mean, SD, var,
min, max, and number of trials
}
\description{
Returns descriptive statistics about a column of choice. A simple convenience function that wraps
\code{dplyr::group_by} and \code{dplyr::summarize}, allowing a quick glance at the data.
}
\examples{
data(word_recognition)
data <- make_eyetrackingr_data(word_recognition, 
                               participant_column = "ParticipantName",
                               trial_column = "Trial",
                               time_column = "TimeFromTrialOnset",
                               trackloss_column = "TrackLoss",
                               aoi_columns = c('Animate','Inanimate'),
                               treat_non_aoi_looks_as_missing = TRUE
)
describe_data(data, describe_column = "Animate", group_columns = "ParticipantName")
 
}
