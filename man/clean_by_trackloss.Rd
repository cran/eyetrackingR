% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep.R
\name{clean_by_trackloss}
\alias{clean_by_trackloss}
\title{Clean data by removing high-trackloss trials/subjects.}
\usage{
clean_by_trackloss(
  data,
  participant_prop_thresh = 1,
  trial_prop_thresh = 1,
  window_start_time = -Inf,
  window_end_time = Inf
)
}
\arguments{
\item{data}{Data already run through \code{make_eyetrackingr_data}}

\item{participant_prop_thresh}{Maximum proportion of trackloss for participants}

\item{trial_prop_thresh}{Maximum proportion of trackloss for trials}

\item{window_start_time, window_end_time}{Time-window within which you want trackloss analysis to
be based. Allows you to keep the entire trial window for data, but clean based on the trackloss
within a subset of it}
}
\value{
Cleaned data
}
\description{
Remove trials/participants with too much trackloss, with a customizable threshold.
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

# scrub all trials with greater than 25\% trackloss, and all
# participants with greater than 25\% trackloss on average
# during the timeperiod 15500-2100
data_clean <- clean_by_trackloss(data,
                                 participant_prop_thresh = .25, 
                                 trial_prop_thresh = .25,
                                 window_start_time = 15500, 
                                 window_end_time = 21000
)

# scrub all trials with greater than 25\% trackloss, but leave participants with a high average
data_clean <- clean_by_trackloss(data,
                                 trial_prop_thresh = .25,
                                 window_start_time = 15500, 
                                 window_end_time = 21000
)
  
}
