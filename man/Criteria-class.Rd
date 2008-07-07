\name{Criteria-class}
\docType{class}
\alias{Criteria-class}
\title{Class "Criteria" for Winner Selection}
\description{This class represents a set of filtering criteria.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Criteria", ...)}, but most often as the result of a call to \code{\link{Criteria}}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}. The test names.}
    \item{\code{operator}:}{Object of class \code{"character"}. The comparison operators.}
    \item{\code{values}:}{Object of class \code{"list"}. List of objects of class \code{"numeric"}. The cutoff values.}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Criteria", i = "character", j = "missing")}: Extract a subset of the criteria.}
    \item{[}{\code{signature(x = "Criteria", i = "character", j = "numeric")}: Extract a subset of the criteria.}
    \item{[}{\code{signature(x = "Criteria", i = "logical", j = "missing")}: Extract a subset of the criteria.}
    \item{[}{\code{signature(x = "Criteria", i = "logical", j = "numeric")}: Extract a subset of the criteria.}
    \item{[}{\code{signature(x = "Criteria", i = "numeric", j = "missing")}: Extract a subset of the criteria.}
    \item{[}{\code{signature(x = "Criteria", i = "numeric", j = "numeric")}: Extract a subset of the criteria.}
    \item{[}{\code{signature(x = "Criteria", i = "missing", j = "missing")}: Extract a subset of the criteria.}
    \item{[}{\code{signature(x = "Criteria", i = "missing", j = "numeric")}: Extract a subset of the criteria.}
    \item{[[}{\code{signature(x = "Criteria", i = "numeric", j = "missing")}: Get a combination of the criteria.}
    \item{[<-}{\code{signature(x = "Criteria", i = "character", j = "missing", value = "Criteria")}: Replace a subset of the criteria.}
    \item{[<-}{\code{signature(x = "Criteria", i = "character", j = "numeric", value = "numeric")}: Replace a subset of the criteria.}
    \item{[<-}{\code{signature(x = "Criteria", i = "logical", j = "missing", value = "Criteria")}: Replace a subset of the criteria.}
    \item{[<-}{\code{signature(x = "Criteria", i = "logical", j = "numeric", value = "numeric")}: Replace a subset of the criteria.}
    \item{[<-}{\code{signature(x = "Criteria", i = "numeric", j = "missing", value = "Criteria")}: Replace a subset of the criteria.}
    \item{[<-}{\code{signature(x = "Criteria", i = "numeric", j = "numeric", value = "numeric")}: Replace a subset of the criteria.}
    \item{[<-}{\code{signature(x = "Criteria", i = "missing", j = "missing", value = "Criteria")}: Replace a subset of the criteria.}
    \item{[<-}{\code{signature(x = "Criteria", i = "missing", j = "numeric", value = "numeric")}: Replace a subset of the criteria.}
    \item{as.character}{\code{signature(x = "Criteria")}: Convert to "character".}
    \item{c}{\code{signature(x = "Criteria", recursive = "missing")}: Concatenation.}
    \item{coerce}{\code{signature(from = "Criteria", to = "character")}: Class coersion.}
    \item{coerce}{\code{signature(from = "character", to = "Criteria")}: Class coersion.}
    \item{length}{\code{signature(x = "Criteria")}: Number of criteria.}
    \item{names}{\code{signature(x = "Criteria")}: Column names.}
    \item{show}{\code{signature(object = "Criteria")}: Display object briefly.}
  }
}
\keyword{classes}
