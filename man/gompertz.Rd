% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{gompertz}
\alias{gompertz}
\title{Gompertz–Makeham intensity function.}
\usage{
gompertz(alpha, beta, lambda = 0)
}
\arguments{
\item{alpha}{Non-negative real parameter.}

\item{beta}{Non-negative real parameter.}

\item{lambda}{Non-negative real parameter.}
}
\value{
Function which associates \code{x} to \eqn{\alpha exp(\beta x) + \lambda}.
}
\description{
The intensity function (or hazard function) for the Gompertz-Makeham law of mortality distribution is defined as
\deqn{h(x) = \alpha e^{\beta x} + \lambda}
with \eqn{\alpha, \beta, \lambda \in {R}_+}.
}
\details{
A C++ version of this function is available. See \code{vignette('IBMPopSim_cpp')} for more details.
}
\seealso{
\url{https://en.wikipedia.org/wiki/Gompertz\%E2\%80\%93Makeham_law_of_mortality}
}
