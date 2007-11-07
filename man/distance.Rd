\name{distance}
\alias{distance}
\title{Wfccm Distance}
\description{
  Run distance on a set of sample scores.
}
\usage{
  distance(scores, group = NULL, group.1 = NA, group.2 = NA, verbose = FALSE)

}
\arguments{
  \item{scores}{numeric vector of scores for a set of samples.}
  \item{group}{factor or numeric vector of classifications for the samples, respectively.  Required for a training cohort; provides accuracy for a testing cohort.}
  \item{group.1}{mean (or center) of group 1 in the training data.  This is necessary for classifying a testing cohort.}
  \item{group.2}{mean (or center) of group 2 in the training data.  This is necessary for classifying a testing cohort.}
  \item{verbose}{boolean. If \code{TRUE}, include full table of output; otherwise, just the classification predictions.}
}
\details{
  For a training cohort, pass the scores and group classification
  \preformatted{
distance(scores, group, \dots)
}

  For a testing cohort, pass the means of the two groups from the training cohort
  \preformatted{
distance(scores, group.1, group.2, \dots)
}

  Uses simple distance on a set of data to do two-class classification.  For training data, you only need the scores and true classification.  For testing data, you only need the scores and group means of the training data.  Optionally, you may include the true classification for the testing data to get classification accuracy.
}
\value{
  \item{prediction}{A factor of class predictions for each sample in the list of scores.  If verbose, a data.frame of scores, predicted class, group means, distances from group means, and possibly a flag for correct classification.}
  \item{accuracy}{An array with overall, group 1, and group 2 accuracy.}
}
\author{
  Will Gray\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{w.gray@vanderbilt.edu}
}
\examples{
  \dontshow{
    x <- rnorm(10)
    y <- 1 * (rnorm(10) > 0)
    z <- rnorm(10)
  }
  y <- factor(y)
  lvls <- levels(y)
  distance(x, y)
  distance(z, group.1 = mean(x[y == lvls[1]]), group.2 = mean(x[y == lvls[2]]))
}
\keyword{models}
\concept{classification}
