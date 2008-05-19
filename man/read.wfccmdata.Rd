\name{read.wfccmdata}
\alias{read.wfccmdata}
\alias{read.wfccminfo}
\title{Data Input}
\description{
  Reads a file in \emph{WFCCM2} data or clinical info format.
}
\usage{
read.wfccmdata(file, sep = "\t", \dots)
read.wfccminfo(file, sep = "\t", \dots)
}
\arguments{
  \item{file}{the name of the file which the data are to be read from.  If it does not contain an \emph{absolute} path, the file name is \emph{relative} to the current working directory, \code{getwd()}.  Tilde-expansion is performed where supported.}
  \item{sep}{the field separator character.  Values on each line of the file are separated by this character.  If \code{sep = ""} the separator is \dQuote{white space}, that is one or more spaces, tabs, newlines or carriage returns.}
  \item{\dots}{additional arguments of \code{\link{read.table}}.}
}
\details{
  This function is just a wrapper to \code{\link{read.table}} that gives some defaults for certain arguments based on the format used in \emph{WFCCM2}.

  In the case of \code{read.wfccmdata}, it also removes the id field and transposes the data so fields (variables) are columns and records (samples) are rows.
}
\keyword{file}
\keyword{connection}
