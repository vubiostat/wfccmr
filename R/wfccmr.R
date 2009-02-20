wfccmr <- function(data, group, tests=tests.wfccm(data,group), criteriamanager, testdata=NULL, testgroup=NULL, verbose=TRUE) {
    group <- factor(group)
    if (nlevels(group) != 2)
        stop("group information should have 2 levels")
    results <- list()
    for (i in 1:length(criteriamanager)) {
        result <- list()
        # get winners for criteriaset
        winners <- winners.wfccm(tests, criteriamanager[[i]])
        tests.filt <- cbind(tests, winners[,-1])[winners$pass,]
        # filter data
        data.filt <- subset(data, select=winners$pass)
        # calculate patient scores
        tests.filt <- sign.wfccm(tests.filt, criteriamanager@sign[1], criteriamanager@sign[-1])
        scores <- scores.wfccm(data.filt, tests.filt, criteriamanager@wfccmfunction)
        winners <- cbind(winners, wfccmScore=NA)
        winners[winners$pass, ]$wfccmScore <- scores$features
        # run distance
        result$training <- distancep(scores$samples, group, verbose=verbose, permutations=criteriamanager@permutations)
        # save winners
        result$winners <- winners
        # ROC
        result$roc.training <- roc(scores$samples, group=group)
        # testing
        if (!is.null(testdata)) {
            # filter testing data
            testdata.filt <- subset(testdata, select=winners$pass)
            # calculate testing patient scores
            scores.test <- scores.wfccm(testdata.filt, features=scores$features)
            # run testing distance
            result$testing <- distancep(scores.test$samples, testgroup, group.1=result$training$group.1, group.2=result$training$group.2, verbose=verbose, permutations=criteriamanager@permutations)
            # testing ROC
            result$roc.testing <- roc(scores.test$samples, group=testgroup)
        }
        results[[i]] <- result
    }
    return(structure(results, class=c("wfccm", "list")))
}
