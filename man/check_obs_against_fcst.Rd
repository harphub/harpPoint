% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_obs_against_fcst.R
\name{check_obs_against_fcst}
\alias{check_obs_against_fcst}
\title{Observation error check against forecast}
\usage{
check_obs_against_fcst(
  .fcst,
  parameter,
  num_sd_allowed = NULL,
  stratification = c("SID", "quarter_day")
)
}
\arguments{
\item{.fcst}{A \code{harp_df} data frame, or a \code{harp_list}, with an observations
column.}

\item{parameter}{The observations column. Can be the column name, quoted, or
unquoted. If a variable it should be embraced - i.e. wrapped in \code{{{}}}.}

\item{num_sd_allowed}{The number of standard deviations of the forecast that
the difference between the forecast and the observation must be smaller
than.}

\item{stratification}{The columns to stratify the data by when computing the
allowed tolerance. In most cases the column must exist in the input data,
but "quarter_day" can be passed to divide the observations into classes of
[0, 6), [6, 12), [12, 18) and [18, 24) hour of day. The default behaviour
is to stratify by station ("SID") and "quarter_day".}
}
\value{
An object of the sames class as \code{.fcst} with an attribute named
\code{removed_cases} containing a data frame of the removed cases.
}
\description{
For each stratification the standard deviation of the forecast over all
forecast cycles and models (mname), and in the case of an eps forecast, the
ensemble members is computed. The difference between the observation and the
forecast is expected to be smaller than a number of multiples of the standard
deviation. The number of multiples of the standard deviation can be supplied
or a default value used depending on the parameter.
}
