\name{CriteriaSet}
\alias{CriteriaSet}
\alias{is.CriteriaSet}
\title{CriteriaSet objects}
\description{
  Create or test for objects of type \code{"CriteriaSet"}.
}
\usage{
CriteriaSet(criteria = Criteria(), pass = "")
is.CriteriaSet(x)
}
\arguments{
  \item{criteria}{object of type \code{"Criteria"}}
  \item{pass}{character string.  A function to help determine winners.}
  \item{x}{object to be coerced or tested.}
}
\value{
\code{CriteriaSet} creates an object of type CriteriaSet.

\code{is.CriteriaSet} returns \code{TRUE} or \code{FALSE} depending on whether its argument is of CriteriaSet type or not.
}
\seealso{
  \code{\link{CriteriaSet-class}}
}
\examples{
}
\keyword{classes}
\keyword{methods}
