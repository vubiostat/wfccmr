\name{TopNManager-class}
\docType{class}
\alias{TopNManager-class}
\title{Class "TopNManager" for Winner Selection and Distance}
\description{This class represents a set of filtering criteria for WFCCM distance. The Top N features are selected based on overall rank.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TopNManager", ...)}, but most often as the result of a call to \code{\link{TopNManager}}.
}
\section{Slots}{
	 \describe{
    \item{\code{topN}:}{Object of class \code{"numeric"}. The levels of overall rank to use, 1-N.}
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
    \item{[[}{\code{signature(x = "TopNManager", i = "numeric", j = "missing")}: Extract a set of criteria for the top N.}
    \item{as.character}{\code{signature(x = "TopNManager")}: Convert to "character".}
    \item{coerce}{\code{signature(from = "TopNManager", to = "character")}: Class coersion.}
    \item{length}{\code{signature(x = "TopNManager")}: The value for topN.}
    \item{length<-}{\code{signature(x = "TopNManager", value = "numeric")}: Replace the value for topN.}
    \item{show}{\code{signature(object = "TopNManager")}: Display the object briefly.}
	 }
}
\examples{
showClass("TopNManager")
}
\keyword{classes}
