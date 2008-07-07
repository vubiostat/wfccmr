\name{CutoffManager-class}
\docType{class}
\alias{CutoffManager-class}
\title{Class "CutoffManager" for Winner Selection and Distance}
\description{This class represents a set of cutoff filtering criteria for WFCCM distance. The features are selected based on criteria cutoffs.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CutoffManager", ...)}, but most often as the result of a call to \code{\link{CutoffManager}}.
}
\section{Slots}{
  \describe{
    \item{\code{numPass}:}{Object of class \code{"numeric"}. The cutoff levels for numPass.}
    \item{\code{fdrPass}:}{Object of class \code{"numeric"}. The cutoff levels for fdrPass.}
    \item{\code{criteria}:}{Object of class \code{"Criteria"}. The filtering criteria.}
    \item{\code{name}:}{Object of class \code{"character"}. The set name.}
    \item{\code{sign}:}{Object of class \code{"character"}. The sign order.}
    \item{\code{wfccmfunction}:}{Object of class \code{"character"}. The WFCCM function.}
    \item{\code{prefilter}:}{Object of class \code{"character"}. The pre-filter function.}
    \item{\code{permutations}:}{Object of class \code{"numeric"}. The number of distance permutations to run.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{CriteriaManager}"}, directly.
}
\section{Methods}{
  \describe{
    \item{[[}{\code{signature(x = "CutoffManager", i = "numeric", j = "missing")}: Extract a combination of the criteria.}
    \item{as.character}{\code{signature(x = "CutoffManager")}: Convert to "character".}
    \item{coerce}{\code{signature(from = "CutoffManager", to = "character")}: Class coersion.}
    \item{length}{\code{signature(x = "CutoffManager")}: The number of criteria combinations.}
    \item{show}{\code{signature(object = "CutoffManager")}: Display object briefly.}
  }
}
\examples{
showClass("CutoffManager")
}
\keyword{classes}
