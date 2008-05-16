\name{sign.wfccm}
\alias{sign.wfccm}
\title{WFCCM Sign Data}
\description{
  Applies signs to data.frame variables for use in WFCCM.
}
\usage{
sign.wfccm(data, lead, rest)
}
\arguments{
  \item{data}{numeric data.frame.}
  \item{lead}{numeric or character of the index or name of the variable to get the sign to apply to the \code{rest} variables.}
  \item{rest}{numeric or character of the indices or names of the variables to apply the \code{lead} sign to.}
}
\details{
  Applies the signs of the values in column \code{lead} to the absolute value of the values in columns \code{rest}.
}
\value{
  A copy of \code{data} with the WFCCM sign method applied.
}
\keyword{utilities}
