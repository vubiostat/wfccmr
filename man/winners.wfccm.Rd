\name{winners.wfccm}
\alias{winners.wfccm}
\title{Select WFCCM Winners}
\description{
Uses a CriteriaSet to select the winners from a collection of test statistics and p-values.
}
\usage{
winners.wfccm(stats, criteriaset)
}
\arguments{
  \item{stats}{numeric data.frame.}
  \item{criteriaset}{object of type CriteriaSet.}
}
\details{
Evaluates each criteria in \code{criteriaset}, determines the numPass and fdrPass, then evaluates the pass function in \code{criteriaset}.
}
\value{
  A data.frame with three columns.
  \item{pass}{boolean vector of which features passed as winners.}
  \item{fdrPass}{numeric vector of the number of fdr cutoffs passed.}
  \item{numPass}{numeric vector of the number of cutoffs passed.}
}
\keyword{utilities}
