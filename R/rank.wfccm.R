rank.wfccm <- function(data, rev=FALSE, ties.break=NULL) {
    if (!is.logical(rev))
        stop("Parameter rev must be of mode logical.")

    data <- as.data.frame(data)
    ranks <- as.data.frame(matrix(0, nrow(data), 0), row.names(data))
    cols <- colnames(data)
    rev <- c(rev, rep(FALSE, ncol(data) - length(rev)))

    ranks <- mapply(function(x, r, n) {
        l <- nchar(n)
        if (substr(n, l-4, l) == ".rank")
            x
        else
            order(order(x, r))
    }, data, rev, cols)

    return(data.frame(ranksum=rowSums(ranks),
        rank=if (missing(ties.break))
            rank(ranks$ranksum)
        else
            order(order.data.frame(data.frame(ranks$ranksum, data[,ties.break])))
    ))
}
