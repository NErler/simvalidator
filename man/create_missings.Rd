% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_missingness.R
\name{create_missings}
\alias{create_missings}
\title{Create missing values in a \code{data.frame}}
\usage{
create_missings(
  data,
  idvars,
  prop_mis,
  prop_cc,
  patterns = NULL,
  weights = NULL,
  type = "RIGHT",
  groups = NULL,
  varlvls = NULL
)
}
\arguments{
\item{data}{a complete \code{data.frame}}

\item{prop_mis}{a named vector indicating the proportion of missing values
per variable (only variables with proportions > 0 have to be
included)}

\item{prop_cc}{the proportion of complete cases}

\item{patterns}{a matrix specifying the missing data patterns to be used; a
\code{1} indicates a variable is completely observed and \code{0} that
it is incomplete; if unspecified, this is created
automatically.}

\item{weights}{a matrix of the same dimension as \code{patterns} that indicates
which variables are predictive of missingness in this pattern;
this argument allows to define MCAR, MAR or MNAR missingness.
If not specified, \code{weights} is set equal to \code{patterns}, i.e.,
MAR with all observed variables being predictors.}
}
\description{
Create missing values in a \code{data.frame}
}
