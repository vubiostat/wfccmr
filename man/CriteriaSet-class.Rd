\name{CriteriaSet-class}
\docType{class}
\alias{CriteriaSet-class}
\title{Class "CriteriaSet" for Winner Selection}
\description{}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CriteriaSet", ...)}, but most often as the result of a call to \code{\link{CriteriaSet}}.
}
\section{Slots}{
	 \describe{
    \item{\code{criteria}:}{Object of class \code{"Criteria"}. The criteria.}
    \item{\code{pass}:}{Object of class \code{"character"}. The pass function.}
  }
}
\section{Methods}{
  \describe{
    \item{as.character}{\code{signature(x = "CriteriaSet")}: Convert to "character".}
    \item{coerce}{\code{signature(from = "CriteriaSet", to = "character")}: Class coersion.}
    \item{names}{\code{signature(x = "CriteriaSet")}: Column names.}
    \item{show}{\code{signature(object = "CriteriaSet")}: Display object briefly.}
	 }
}
\examples{
showClass("CriteriaSet")
}
\keyword{classes}
