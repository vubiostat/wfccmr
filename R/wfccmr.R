wfccmr <- function(data, group, tests=tests.wfccm(data,group), model, testdata=NULL, testgroup=NULL, ...)
{
    group <- factor(group)
    lvls <- levels(group)
    if (length(lvls) != 2)
        stop("group information should have 2 levels")
    results <- list()
    for (i in 1:length(model)) {
        # get winners for model
        winners <- winners.wfccm(tests, model[[i]])
        # filter data
        tests.filt <- cbind(tests, winners[,-1])[winners$pass,]
        data.filt <- data[, winners$pass, drop=FALSE]
        # calculate patient scores
        scores <- scores.wfccm(data.filt, tests.filt, model)
        winners <- cbind(winners, wfccmScore=NA)
        winners[winners$pass, ]$wfccmScore <- scores$features
        # run distance
        prediction.train <- distance(scores$samples, group, ...)
        result <- list()
        result$winners <- winners
        result$training <- prediction.train
        # ROC
        result$roc.training <- roc(scores$samples, group=group)
        # testing
        if (!missing(testdata)) {
            # filter testing data
            testdata.filt <- testdata[, winners$pass, drop=FALSE]
            # calculate testing patient scores
            scores.test <- scores.wfccm(testdata.filt, features=scores$features)
            # run testing distance
            result$testing <- distancep(scores.test$samples, testgroup, group.1=result$training$group.1, group.2=result$training$group.2, verbose=verbose, permutations=criteriamanager@permutations)
            # testing ROC
            result$roc.testing <- roc(scores.test$samples, group=testgroup)
        }
        results[[i]] <- result
    }
    return(results)
}
