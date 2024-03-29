% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep.R
\name{add_aoi}
\alias{add_aoi}
\title{Add an area-of-interest to your dataset, based on x-y coordinates and the AOI rectangle.}
\usage{
add_aoi(
  data,
  aoi_dataframe,
  x_col,
  y_col,
  aoi_name,
  x_min_col = "L",
  x_max_col = "R",
  y_min_col = "T",
  y_max_col = "B"
)
}
\arguments{
\item{data}{Your data}

\item{aoi_dataframe}{A dataframe specifying the bounding-box for the AOI}

\item{x_col, y_col}{What are the column names for the x and y coordinates in your dataset?}

\item{aoi_name}{What is the name of this AOI?}

\item{x_min_col, x_max_col}{What are the column names for the left and right edge of the AOI-bounding box?
Default "L","R"}

\item{y_min_col, y_max_col}{What are the column names for the top and bottom edge of the AOI-bounding box?
Default "T","B"}
}
\value{
Dataset with a new column indicating whether gaze is in the AOI
}
\description{
Eyetracking-R requires that there is a column for each area-of-interest, specifying whether the gaze is 
within that area for each sample. This function creates an AOI column if needed.
}
\details{
Many eyetracking software packages export your data with a column corresponding to each AOI; however, if
your software does not do this, or if you had to define or revise your AOIs after running the experiment,
then this function will add the necessary AOI columns for you. The function takes two dataframes: (1) your
original data, (2) a dataframe specifying the bounding box for the AOI. The latter can specify a different
bounding box for each trial, each subject, each image, or even each video-frame-- anything you like. The
two dataframes are simply joined by matching any columns they have in common (case sensitive!), so if
there's a unique AOI for each "Trial" in the \code{aoi_dataframe}, and there's a "Trial" column in the
\code{data} dataframe, then the unique AOI coordinates for each trial will be used.
}
