rank.wfccm <- function(data, rev=FALSE, ties.break=NULL)
{
    if (mode(rev) != mode(logical()))
        stop('Parameter rev must be of mode logical.')

    data <- as.data.frame(data)
    ranks <- data.frame(matrix(0, dim(data)[1], 0))
    row.names(ranks) <- row.names(data)
    cols <- colnames(data)
    rev <- c(rev, rep(FALSE, dim(data)[2] - length(rev)))

    for (i in 1:length(cols))
    {
        n <- cols[i]
        l <- nchar(n)
        if (substr(n, l-4,l) == '.rank')
            ranks[[n]] <- data[,n]
        else
            ranks[[paste(n, 'rank', sep='.')]] <- order(order(data[,i], decreasing=rev[i]))
    }

    ranks$ranksum <- apply(ranks, 1, sum)
    if (missing(ties.break))
        ranks$rank <- rank(ranks$ranksum)
    else
        ranks$rank <- order(order.data.frame(cbind(ranks$ranksum, data[,ties.break])))

    ranks
}
