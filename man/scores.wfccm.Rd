\name{scores.wfccm}
\alias{scores.wfccm}
\title{Compute WFCCM scores}
\description{
  Computes the WFCCM scores that are used in \code{distance}.
}
\usage{
scores.wfccm(data, stats = NULL, wfccmfunction = NULL, features = NULL)
}
\arguments{
  \item{data}{numeric data.frame.}
  \item{stats}{numeric data.frame.}
  \item{wfccmfunction}{character string or expression.  The function that computes the feature scores.}
  \item{features}{numeric vector.}
}
\details{
  If \code{stats} and \code{wfccmfunction} are provided, it prepares the data with \code{scale}, then calculates the feature scores.
}
\value{
  \item{features}{The feature scores based on the WFCCM function.}
  \item{samples}{The sample scores for WFCCM distance.}
}
\keyword{utilities}
