distancep <- function(scores, group, group.1=NA, group.2=NA, verbose=TRUE, permutations=1000) {
    result <- distance(scores, group, group.1, group.2, verbose)
    if (!is.na(permutations) && permutations > 0) {
        cnt <- NA
        for (i in 1:permutations) {
            cnt[i] <- distance(sample(scores), group, group.1, group.2, verbose=FALSE)$accuracy[1]
        }
        result$distancep <- sum(cnt > result$accuracy[1]) / permutations
    } else {
        result$distancep <- NA
    }
    return(result)
}
