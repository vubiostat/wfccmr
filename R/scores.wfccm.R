scores.wfccm <- function(data, stats=NULL, model=NULL, features=NULL)
{
    if ((is.null(model) && is.null(stats)) && is.null(features))
        stop('Must specify either data statistics and model to compute the feature scores or the feature scores themselves.')
    if (all(!is.null(stats), !is.null(model), !is.null(features)))
        stop('Specify either the test statistics and model or the feature scores, but not both.')
    if (is.null(features))
    {
        if (is.null(model))
            stop('Must specify a model to compute the feature scores.')
        else if (is.null(stats))
            stop('Must specify the test statistics to compute the feature scores.')
        if (!'CriteriaManager' %in% class(model))
            stop('Model must be a CriteriaManager')
        names <- colnames(stats)
        std <- names[!is.na(sapply(paste('\\W', names, '.std\\W', sep=''), grep, model@wfccmfunction) == 1)]
        tmp <- cbind(stats, normalize(stats[,std], 2))
        tmp <- sign.wfccm(tmp, model@sign[1], model@sign[2:length(model@sign)])
        features <- with(tmp, eval(parse(text=model@wfccmfunction)))
    }
    if (all(is.na(features)))
        features[1:length(features)] <- rep(0, length(features))
    samples <- as.matrix(data) %*% features
    return(list(features=features, samples=samples))
}
