\name{filter.wfccm}
\alias{filter.wfccm}
\title{WFCCM Filter Data}
\description{
  Evaluates the filter function within the data.frame.
}
\usage{
filter.wfccm(data, func)
}
\arguments{
  \item{data}{data.frame.}
  \item{func}{length-1 character or expression.}
}
\details{
  Uses \code{\link{with}} to evaluate the function on \code{data}.  The result is used to select rows out of \code{data}.
}
\value{
  A subset of \code{data} based on the results from \code{func}.
}
\keyword{utilities}
