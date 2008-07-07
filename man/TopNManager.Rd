\name{TopNManager}
\alias{TopNManager}
\alias{is.TopNManager}
\title{TopNManager objects}
\description{
  Create or test of objects of type \code{"TopNManager"}.
}
\usage{
TopNManager(criteria = Criteria(), name = paste("top", topN, sep = ""),
  sign = "", wfccmfunction = "", prefilter = "", permutations = 0, topN = 100)
is.TopNManager(x)
}
\arguments{
  \item{criteria}{object of type \code{"Criteria"}}
  \item{name}{character string describing the object.}
  \item{sign}{character vector.  The sign rules.}
  \item{wfccmfunction}{character string.  The function to create the score.}
  \item{prefilter}{character string.  The function to filter the data.}
  \item{permutations}{numeric value.  Number of distance permutations to run.}
  \item{topN}{numeric value.  The number of distance models to run.}
  \item{x}{object to be coerced or tested.}
}
\value{
\code{TopNManager} creates an object of type TopNManager.

\code{is.TopNManager} returns \code{TRUE} or \code{FALSE} depending on whether its argument is of TopNManager type or not.
}
\seealso{
  \code{\link{TopNManager-class}}
}
\examples{
}
\keyword{classes}
\keyword{methods}
