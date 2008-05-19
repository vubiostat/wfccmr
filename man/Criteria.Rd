\name{Criteria}
\alias{Criteria}
\alias{as.Criteria}
\alias{is.Criteria}
\title{Criteria objects}
\description{
  Create or test for objects of type \code{"Criteria"}.
}
\usage{
Criteria(name = character(), operator = character(), values = list())
as.Criteria(x)
is.Criteria(x)
}
\arguments{
  \item{name}{character vector of names of the statistics to use.}
  \item{operator}{character vector of operators to use for comparison.}
  \item{values}{list of numeric vectors of cutoff values.}
  \item{x}{object to be coerced or tested.}
}
\details{
\code{as.Criteria} is generic: you can write methods to handle specific classes of objects, see \link{InternalMethods}.

All arguments to \code{Criteria} should have the same length.  A vector for \code{values} will get turned into a list by \code{list} when \code{name} is length 1.  Otherwise, it will be coerced with \code{as.list}.
}
\value{
\code{Criteria} creates an object of type Criteria.

\code{as.Criteria} attempts to coerce its argument to Criteria type.

\code{is.Criteria} returns \code{TRUE} or \code{FALSE} depending on whether its argument is of Criteria type or not.
}
\section{S4 methods}{
\code{as.Criteria} is S4 generic so methods can be set for it \emph{via} \code{setMethod}.
}
\seealso{
  \code{\link{Criteria-class}}
}
\examples{
(crits <- c(Criteria("asam", ">", c(2,4,6)),
            Criteria("prob.t", "<", c(0.01, 0.001))))
as.Criteria("asam > 5")
}
\keyword{classes}
\keyword{methods}
