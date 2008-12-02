\name{wfccmr}
\alias{wfccmr}
\alias{cv.wfccmr}
\title{WFCCMR}
\description{
  Runs all steps for WFCCM.
}
\usage{
wfccmr(data, group, tests = tests.wfccm(data, group), criteriamanager,
  testdata = NULL, testgroup = NULL, verbose = TRUE)

cv.wfccmr(data, group, strata = stratify(group, n = 10), tests = expression(tests.wfccm(data.in, group.in)),
criteriamanager, testdata = NULL, testgroup = NULL, verbose = TRUE)
}
\arguments{
  \item{data}{numeric matrix or data.frame.}
  \item{group}{2-level factor of group classification.}
  \item{strata}{multi-level factor of cross-validation folds for \code{cv.wfccmr}.}
  \item{tests}{numeric matrix or data.frame of statistical test values and p-values for \code{wfccm}.  Optionally, an \code{expression} for \code{cv.wfccmr} that gets evaluated for each fold.}
  \item{criteriamanager}{object of class \code{\link{CriteriaManager}}.}
  \item{testdata}{numeric matrix or data.frame.  Optional.}
  \item{testgroup}{2-level factor.  Optional.}
  \item{verbose}{boolean.  Whether or not \code{distance} should produce verbose table output or not.}
}
\details{
}
\value{
}
\keyword{utilities}
