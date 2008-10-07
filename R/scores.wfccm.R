scores.wfccm <- function(data, stats=NULL, wfccmfunction=NULL, features=NULL) {
    if (is.null(stats) && is.null(wfccmfunction) && is.null(features))
        stop("Must specify either test statistics and wfccmfunction to compute the feature scores or the feature scores themselves.")
    if (!is.null(stats) && !is.null(wfccmfunction) && !is.null(features))
        stop("Specify either the test statistics and wfccmfunction or the feature scores, but not both.")
    if (is.null(features)) {
        if (is.null(wfccmfunction))
            stop("Must specify a wfccmfunction to compute the feature scores.")
        else if (is.null(stats))
            stop("Must specify the test statistics to compute the feature scores.")
        if (!is.character(wfccmfunction) || length(wfccmfunction) != 1)
            stop("wfccmfunction must be a single character string.")
        names <- colnames(stats)
        std <- names[!is.na(sapply(paste("\\W", names, ".std\\W", sep=""), grep, wfccmfunction) == 1)]
        tmp <- data.frame(scale(stats[,std]))
        colnames(tmp) <- paste(colnames(tmp), "std", sep=".")
        features <- with(cbind(stats, tmp), eval(parse(text=wfccmfunction)))
    }
    if (all(is.na(features)))
        features[1:length(features)] <- numeric(length(features))
    samples <- as.vector(as.matrix(data) %*% features)
    return(list(features=features, samples=samples))
}
