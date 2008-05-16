\name{scores.wfccm}
\alias{scores.wfccm}
\title{Compute WFCCM scores}
\description{
  Computes the WFCCM scores that are used in \code{distance}.
}
\usage{
scores.wfccm(data, stats = NULL, model = NULL, features = NULL)
}
\arguments{
  \item{data}{numeric data.frame.}
  \item{stats}{numeric data.frame.}
  \item{model}{object of type CriteriaManager.}
  \item{features}{numeric vector.}
}
\details{
  If \code{stats} and \code{model} are provided, it prepares the data with \code{normalize} and \code{sign.wfccm}, then calculates the feature scores.
}
\value{
  \item{features}{The feature scores based on the WFCCM function.}
  \item{samples}{The sample scores for WFCCM distance.}
}
\keyword{utilities}
