rank.wfccm <- function(data, rev=FALSE, ties.break=NULL) {
    if (!is.logical(rev))
        stop("Parameter rev must be of mode logical.")

    data <- as.data.frame(data)
    ranks <- as.data.frame(matrix(0, nrow(data), 0), row.names(data))
    cols <- colnames(data)
    rev <- c(rev, rep(FALSE, ncol(data) - length(rev)))

    ranks <- as.data.frame(mapply(function(x, r, n) {
        l <- nchar(n)
        if (substr(n, l-4, l) == ".rank")
            x
        else
            order(order(x, r))
    }, data, rev, cols))
    which <- grep("\\.rank$", cols) != 1:length(cols)
    if (any(which)) {
        colnames(ranks)[which] <- paste(cols[which], "rank", sep=".")
    }

    ranksum <- rowSums(ranks)
    allrank <- if (missing(ties.break) || is.null(ties.break)) {
        rank(ranksum)
    } else {
        order(order.data.frame(data.frame(ranksum, subset(data, select=ties.break))))
    }

    return(data.frame(ranks, ranksum, rank=allrank))
}
