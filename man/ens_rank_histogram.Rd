% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ens_rank_histogram.R
\name{ens_rank_histogram}
\alias{ens_rank_histogram}
\alias{ens_rank_histogram.harp_ens_point_df}
\title{Rank histogram for an ensemble.}
\usage{
ens_rank_histogram(
  .fcst,
  parameter,
  groupings = "lead_time",
  jitter_fcst = NULL,
  show_progress = TRUE,
  ...
)

\method{ens_rank_histogram}{harp_ens_point_df}(
  .fcst,
  parameter,
  groupings = "lead_time",
  jitter_fcst = NULL,
  show_progress = TRUE,
  fcst_model = NULL,
  ...
)
}
\arguments{
\item{.fcst}{A \code{harp_df} or \code{harp_list} object with tables that have a column
for observations, or a single forecast table.}

\item{parameter}{The name of the column for the observations data. Can be the
column name, quoted, or unquoted. If a variable it should be embraced -
i.e. wrapped in \code{{{}}}.}

\item{groupings}{The groups for which to compute the scores. See
\link[dplyr]{group_by} for more information of how grouping works.}

\item{jitter_fcst}{A function to perturb the forecast values by. This is used
to account for observation error in the rank histogram. For other
statistics it is likely to make little difference since it is expected that
the observations will have a mean error of zero.}

\item{show_progress}{Logical - whether to show progress bars. Defaults to
\code{TRUE}.}

\item{...}{Reserved for methods.}

\item{fcst_model}{The name of the forecast model to use in the \code{fcst_model}
column of the output. If the function is dispatched on a \code{harp_list}
object, the names of the \code{harp_list} are automatically used.}
}
\value{
An object of the same format as the inputs but with data grouped for
the \code{groupings} column(s) and columns for \code{rank} and
\code{rank_count} that are nested together in a column with the name
\code{rank_histogram}.
}
\description{
The rank histogram is computed as columns in a \code{harp_list} object.
Typically the scores are aggregated over lead time, but other grouping
variables can be chosen.
}
