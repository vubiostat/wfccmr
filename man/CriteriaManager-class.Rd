\name{CriteriaManager-class}
\docType{class}
\alias{CriteriaManager-class}
\title{Class "CriteriaManager" for Winner Selection and Distance}
\description{This class represents a set of filtering criteria for WFCCM distance.}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
	 \describe{
    \item{\code{criteria}:}{Object of class \code{"Criteria"}. The filtering criteria.}
    \item{\code{name}:}{Object of class \code{"character"}. The set name.}
    \item{\code{sign}:}{Object of class \code{"character"}. The sign order.}
    \item{\code{wfccmfunction}:}{Object of class \code{"character"}. The WFCCM function.}
    \item{\code{prefilter}:}{Object of class \code{"character"}. The pre-filter function.}
    \item{\code{permutations}:}{Object of class \code{"numeric"}. The number of distance permutations to run.}
  }
}
\section{Methods}{
  \describe{
    \item{[[}{\code{signature(x = "CriteriaManager", i = "ANY", j = "ANY")}: Extract a combination of the criteria.}
    \item{as.character}{\code{signature(x = "CriteriaManager")}: Convert to "character".}
    \item{coerce}{\code{signature(from = "CriteriaManager", to = "character")}: Class coersion.}
    \item{length}{\code{signature(x = "CriteriaManager")}: The number of criteria combinations. Not supported.}
    \item{length<-}{\code{signature(x = "CriteriaManager", value = "ANY")}: Set the number of criteria combinations. Not supported.}
    \item{names}{\code{signature(x = "CriteriaManager")}: Get set name.}
    \item{names<-}{\code{signature(x = "CriteriaManager", value = "character")}: Replace the set name.}
    \item{show}{\code{signature(object = "CriteriaManager")}: Display object briefly.}
	 }
}
\examples{
showClass("CriteriaManager")
}
\keyword{classes}
