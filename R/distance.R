distance <- function(scores, group=NULL, group.1=NA, group.2=NA, verbose=FALSE) {
    # will we have to calculate the centers of the groups?
    calc.group <- (is.na(group.1) || is.na(group.2))

    # no group info provided to calculate groups
    if (is.null(group) && calc.group) {
        # forgot one
        if (!is.na(group.1) || !is.na(group.2)) {
            stop("Need center of both groups for testing.")
        # forgot both or the group information
        } else {
            stop("Need group information for training or center of training groups for testing.")
        }
    }
    # only provided one group"s center, but included group information
    else if (!is.null(group) && xor(is.na(group.1), is.na(group.2))) {
        warning("Need center of both groups for testing.  Doing training instead.")
    }

    # should have group information for each score
    if (!is.null(group) && (length(scores) != length(group))) {
        stop("Length of scores and group information do not match.")
    }

    # convert group information to factor
    if (!is.null(group)) {
        group <- factor(group)
        lvls <- levels(group)
        n <- summary(group)
    } else {
        lvls <- c(1,2)
    }

    # calculate the centers of groups
    if (calc.group) {
        means <- by(scores, group, mean)
        mean.1 <- means[1]
        mean.2 <- means[2]
        group.1 <- ifelse(group == lvls[1], (mean.1 * n[1] - scores) / (n[1] - 1), mean.1)
        group.2 <- ifelse(group == lvls[2], (mean.2 * n[2] - scores) / (n[2] - 1), mean.2)
    } else {
        mean.1 <- group.1
        mean.2 <- group.2
    }

    # distance from groups and classify scores
    dist.1 <- abs(group.1 - scores)
    dist.2 <- abs(group.2 - scores)
    result <- ifelse(dist.1 == dist.2 | is.na(dist.1) | is.na(dist.2) | is.na(scores), NA, lvls[(dist.1 > dist.2) + 1])

    # build output
    #   verbose = full table
    final <- list()
    result <- factor(result, lvls)
    if (verbose) {
        final$prediction <- data.frame( Score=scores,
                                        Calc.Group=result,
                                        Mean1=group.1,
                                        Dist1=dist.1,
                                        Dist2=dist.2,
                                        Mean2=group.2)
    } else {
        final$prediction <- result
    }
    final$group.1 <- mean.1
    final$group.2 <- mean.2
    if (!is.null(group)) {
        final$accuracy <- accuracy(group, result)
        final$n <- c(total=sum(n), n)
        if (verbose) {
            final$prediction <- data.frame( final$prediction,
                                            Group=group,
                                            Correct=(group == result))
        }
    }
    return(final)
}
