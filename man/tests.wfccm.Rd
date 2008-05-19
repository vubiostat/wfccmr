\name{tests.wfccm}
\alias{tests.wfccm}
\title{WFCCM Statistical Tests}
\description{
  Runs and collates statistical tests for use in WFCCM.
}
\usage{
tests.wfccm(x, grp, tests = c("t", "ks", "wilcox", "sam", "wga", "huwright", "info"), ...)
}
\arguments{
  \item{x}{numeric matrix with rows as samples and columns as variables.}
  \item{grp}{2-level factor of group classification.}
  \item{tests}{character vector of \code{X.test} names.  (e.g. t, ks, wilcox)}
  \item{\dots}{Additional parameters for the tests.}
}
\details{
  This is a helper function to automate the collation of statistical test results.
}
\value{
  A \code{data.frame} of test statistics and p-values.
}
\keyword{utilities}
