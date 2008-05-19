\name{CriteriaManager}
\alias{CriteriaManager}
\alias{is.CriteriaManager}
\title{CriteriaManager objects}
\description{
  Create or test for objects of type \code{"CriteriaManager"}.
}
\usage{
CriteriaManager(criteria = Criteria(), name = "criteria", sign = "", wfccmfunction = "", prefilter = "", permutations = 0)
is.CriteriaManager(x)
}
\arguments{
  \item{criteria}{object of type \code{"Criteria"}}
  \item{name}{character string describing the object.}
  \item{sign}{character vector.  The sign rules.}
  \item{wfccmfunction}{character string.  The function to create the score.}
  \item{prefilter}{character string.  The function to filter the data.}
  \item{permutations}{numeric value.  Number of distance permutations to run.}
  \item{x}{object to be coerced or tested.}
}
\value{
\code{CriteriaManager} creates an object of type CriteriaManager.

\code{is.CriteriaManager} returns \code{TRUE} or \code{FALSE} depending on whether its argument is of CriteriaManager type or not.
}
\seealso{
  \code{\link{CriteriaManager-class}}
}
\examples{
}
\keyword{classes}
\keyword{methods}
