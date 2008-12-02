cv.wfccmr <- function(data, group, strata=stratify(group, n=10), tests=expression(tests.wfccm(data.in,group.in)), criteriamanager, testdata=NULL, testgroup=NULL, verbose=TRUE) {
    "inc<-" <- function(x, value) { x + value }
    "div<-" <- function(x, value) { x / value }
    group <- factor(group)
    strata <- factor(strata)
    if (nlevels(group) != 2)
        stop("group information should have 2 levels")
    results <- list()
    n <- ncol(data)
    for (i in 1:length(criteriamanager)) {
        result <- list()
        cvresults <- list()
        result$winners <- data.frame(wfccmScore=rep(0, n), count=rep(0,n), row.names=colnames(data))
        for (l in levels(strata)) {
            # divide data for this fold
            select <- (strata == l)
            data.in <- data[!select,]
            data.out <- data[select,]
            group.in <- group[!select]
            group.out <- group[select]
            # get p-values and such
            if (is.expression(tests))
                tests.in <- eval(tests)
            else if (is.character(tests))
                tests.in <- tests.wfccm(data.in, group.in, tests)
            else
                tests.in <- tests[!select,]
            # get winners for criteriamanager
            winners <- winners.wfccm(tests.in, criteriamanager[[i]])
            # filter data
            tests.filt <- cbind(tests.in, winners[,-1])[winners$pass,]
            data.filt <- data[, winners$pass, drop=FALSE]
            data.out.filt <- data.out[, winners$pass, drop=FALSE]
            # calculate patient scores
            tests.filt <- sign.wfccm(tests.filt, criteriamanager@sign[1], criteriamanager@sign[-1])
            scores <- scores.wfccm(data.filt, tests.filt, criteriamanager)
            scores.out <- scores.wfccm(data.out.filt, features=scores$features)
            # save winner scores
            inc(result$winners$wfccmScore[winners$pass]) <- scores$features
            inc(result$winners$count[winners$pass]) <- 1
            # run distance
            dist <- distance(scores$samples, group.in)
            cvresults[[l]] <- distance(scores.out$samples, group.out, group.1=dist$group.1, group.2=dist$group.2, verbose=verbose)
        }
        # compile results
        result$training <- list()
        result$training$prediction <- unsplit(lapply(cvresults, function(x) x$prediction), strata)
        result$training$n <- rowSums(sapply(cvresults, function(x) x$n))
        result$training$accuracy <- rowSums(sapply(cvresults, function(x) x$n * x$accuracy)) / result$training$n
        div(result$winners$wfccmScore) <- nlevels(strata)
        ## testing
        #if (!is.null(testdata)) {
        #    # filter testing data
        #    data.filt <- data[, (result$winners$count > 0), drop=FALSE]
        #    testdata.filt <- testdata[, (result$winners$count > 0), drop=FALSE]
        #    # calculate testing patient scores
        #    scores <- scores.wfccm(data.filt, features=result$winners$wfccmScore)
        #    scores.test <- scores.wfccm(testdata.filt, features=result$winners$wfccmScore)
        #    # run testing distance
        #    temp <- distance(scores$samples, group, verbose=FALSE)
        #    result$testing <- distancep(scores.test$samples, testgroup, group.1=temp$group.1, group.2=temp$group.2, verbose=verbose, permutations=criteriamanager@permutations)
        #    if (!is.null(testgroup)) {
        #        # testing ROC
        #        result$roc.testing <- roc(scores.test$samples, group=testgroup)
        #    }
        #}
        results[[i]] <- result
    }
    return(structure(results, class=c("wfccm", "list")))
}
