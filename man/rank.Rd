\name{rank.wfccm}
\alias{rank.wfccm}
\title{WFCCM Rank Data}
\description{
  Applies rankings to data.frame variables for use in WFCCM.
}
\usage{
  rank.wfccm(data, rev = FALSE, ties.break = NULL)
}
\arguments{
  \item{data}{numeric data.frame or matrix.}
  \item{rev}{logical vector for controlling the rank order.  \code{TRUE} will give rank \code{1} to the largest value in the variable; \code{FALSE} (the default) will give rank \code{1} to the smallest value.}
  \item{ties.break}{optional numeric or character vector of indices or names of the variables to use to break the overall rank tie.}
}
\details{
  Each column of \code{data} is ranked according to the respective value in \code{rev}.  The ranks are summed across rows to create the ranksum, which is then also ranked.
}
\value{
  A data.frame of ranks for the columns in \code{data}.  Two extra columns are also added for the overall rank.
}
\keyword{utilities}
