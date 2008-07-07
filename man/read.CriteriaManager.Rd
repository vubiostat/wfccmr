\name{read.CriteriaManager}
\alias{read.CriteriaManager}
\title{Read CriteriaManager objects}
\description{
  Read objects of type \code{"CriteriaManager"}.
}
\usage{
read.CriteriaManager(file)
}
\arguments{
  \item{file}{the name of the file which the object is to be read from.}
}
\details{
  This is a sample of the format.
  \preformatted{
  <name>

  <prefilter function>

  <wfccm function>

  <sign leader> <sign followers>

  <# of distance permutations>

  <Criteria 1>
  <Criteria 2>
  <Criteria 3>
  \dots
  }
}
\value{
  A CriteriaManager containing the proper parts from the file.
}
\seealso{\code{\link{write.CriteriaManager}}}
\keyword{file}
