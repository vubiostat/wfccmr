\name{CutoffManager}
\alias{CutoffManager}
\alias{is.CutoffManager}
\title{CutoffManager objects}
\description{
  Create or test for objects of type \code{"CutoffManager"}.
}
\usage{
CutoffManager(criteria = Criteria(), name = "cutoff", sign = "",
  wfccmfunction = "", prefilter = "", permutations = 10000,
  numPass = Criteria("numPass", ">=", 1), fdrPass = Criteria("fdrPass", ">=", 1))
is.CutoffManager(x)
}
\arguments{
  \item{criteria}{object of type \code{"Criteria"}}
  \item{name}{character string describing the object.}
  \item{sign}{character vector.  The sign rules.}
  \item{wfccmfunction}{character string.  The function to create the score.}
  \item{prefilter}{character string.  The function to filter the data.}
  \item{permutations}{numeric value.  Number of distance permutations to run.}
  \item{numPass}{object of type \code{"Criteria"}.  Set of cutoffs for the number of criteria to pass.}
  \item{fdrPass}{object of type \code{"Criteria"}.  Set of cutoffs for the number of FDR-adjusted p-value criteria to pass.}
  \item{x}{object to be coerced or tested.}
}
\value{
\code{CutoffManager} creates an object of type CriteriaManager.

\code{is.CutoffManager} returns \code{TRUE} or \code{FALSE} depending on whether its argument is of CutoffManager type or not.
}
\seealso{
  \code{\link{CutoffManager-class}}
}
\examples{
}
\keyword{classes}
\keyword{methods}
