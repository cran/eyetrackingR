% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_cluster_data.R
\name{plot.time_cluster_data}
\alias{plot.time_cluster_data}
\title{Plot test-statistic for each time-bin in a time-series, highlight clusters.
Plot time_cluster_data, highlights clusters of above-threshold time-bins.}
\usage{
\method{plot}{time_cluster_data}(x, type = NULL, ...)
}
\arguments{
\item{x}{The output of \code{make_time_cluster_data}}

\item{type}{This function can plot the test-statistic ("statistic"), the parameter estimate +/-
std. error ("estimate"), the p-value ("pvalue") or the negative-log-pvalue ("neg_log_pvalue").
When test gives critical-statistic, default is to plot the test-statistic. Otherwise, default
is to plot the estimate. For wilcox, only p-values can be plotted; for boot-splines, p-values
cannot be plotted.}

\item{...}{Ignored}
}
\value{
A ggplot object
}
\description{
Plot test-statistic for each time-bin in a time-series, highlight clusters.
Plot time_cluster_data, highlights clusters of above-threshold time-bins.
}
