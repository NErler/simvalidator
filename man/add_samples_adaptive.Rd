% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JM_imp_adaptive.R
\name{add_samples_adaptive}
\alias{add_samples_adaptive}
\title{Add samples adaptively}
\usage{
add_samples_adaptive(
  fitted_model,
  extra_iter = NULL,
  minsize = 500L,
  step = 200L,
  subset = NULL,
  cutoff = 1.2,
  prop = 0.8,
  gr_max = 1.5,
  max_try = 5L
)
}
\arguments{
\item{fitted_model}{a object of class 'JointAI'}

\item{extra_iter}{number of iterations that should be added to the model if
the Gelman-Rubin criterion is too large}

\item{minsize}{the minimum number of iterations to be considered}

\item{step}{the step size in which iterations are omitted as burn-in}

\item{subset}{subset of parameters on which the Gelman-Rubin criterion should
be evaluated. Follows the logic used in \strong{JointAI}}

\item{cutoff}{the cut-off used for the Gelman Rubin criterion}

\item{prop}{proportion of parameters that need to be below the \code{cutoff}}

\item{gr_max}{maximum allowed value for the Gelman-Rubin criterion}

\item{max_try}{maximum number of runs of \code{JointAI::add_samples()}}
}
\description{
Add samples adaptively
}
