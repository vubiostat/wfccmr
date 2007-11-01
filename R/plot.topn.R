plot.topn <- function(x, sub='', show.grid=TRUE, show.legend=TRUE)
{
    l <- length(x)
    f1 <- function(x) x$training$accuracy
    table <- rbind(0, 100 * t(sapply(x, f1)))
    names <- colnames(table)
    ylim <- c(0, 100)
    xlab <- '# of features'
    ylab <- '% correct'
    main <- paste('Top', l)
    lwd <- c(2, rep(1, dim(table)[[2]] - 1))
    matplot(0:l, table, col='blue', type='l', lwd=lwd, ylim=ylim, xlab=xlab, ylab=ylab, main=main, sub='')
    horiz <- TRUE
    ncol <- 1
    f2 <- function(x) x$testing$accuracy
    table2 <- t(sapply(x, f2))
    if (!all(sapply(table2, is.null)))
    {
        table2 <- rbind(0, table2 * 100)
        names <- c(paste(names, 'training'), paste(colnames(table2), 'testing'))
        matplot(0:l, table2, col='red', type='l', lwd=lwd, add=TRUE)
        horiz <- FALSE
        ncol <- dim(table)[[2]]
    }
    if (show.grid)
        abline(h=seq(0,100,10), col='lightgray', lty='dotted')
    if (show.legend)
        legend(x=l, y=0, legend=names, horiz=horiz, ncol=ncol, col=rep(c('blue', 'red'), rep(l, 2)), lwd=lwd, lty=1:l, xjust=1, yjust=0)
}
