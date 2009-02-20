plot.wfccm <- function(x, ...) { plot(summary(x), ...) }

plot.summary.wfccm <- function(x, main=paste("Top", nrow(x)), sub="", xlab="# of winners", ylab="% correct", lwd=NULL, lty=NULL, ann=par("ann"), axes=TRUE, frame.plot=axes, show.grid=axes, show.legend=ann, panel.first=NULL, panel.last=NULL, ...) {

    localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
    localBox <- function(..., col, bg, pch, cex, lty, lwd) box(...)
    localWindow <- function(..., col, bg, pch, cex, lty, lwd) plot.window(...)
    localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)

    each <- 1
    l <- nrow(x)
    n <- ncol(x)
    dp <- grep("^distance\\.p", colnames(x))
    xlim <- c(0, l)
    ylim <- c(0, 100)
    plot.new()
    localWindow(xlim=xlim, ylim=ylim, ...)
    panel.first

    if (show.grid) {
        abline(h=seq(0, 100, 10), col="lightgray", lty="dotted")
    }

    table <- rbind(0, x[, seq(2, dp[1]-1, 2), drop=FALSE])
    names <- colnames(table)

    if (is.null(lwd)) {
        lwd <- c(2, rep(1, ncol(table) - 1))
    }
    if (is.null(lty)) {
        lty <- 1:ncol(table)
    }

    matlines(0:l, table, col="blue", lwd=lwd, lty=lty)

    if (!is.na(dp[2])) {
        each <- 2
        table2 <- rbind(0, x[, seq(dp[1]+1, dp[2]-1, 2), drop=FALSE])
        names <- c(rbind(names, colnames(table2)))
        matlines(0:l, table2, col="red", lwd=lwd, lty=lty)
    }

    panel.last
    if (axes) {
        localAxis(0:l, side=1, ...)
        localAxis(0:100, side=2, ...)
    }
    if (frame.plot)
        localBox(...)
    if (ann)
        localTitle(xlab=xlab, ylab=ylab, main=main, sub="", ...)
    if (show.legend) {
        ncol <- ncol(table)
        legend(x=l, y=0, legend=names, ncol=ncol, col=rep(rep(c("blue","red"), length.out=each), times=ncol), lwd=rep(lwd, each=each), lty=rep(lty, each=each), xjust=1, yjust=0, cex=0.8)
    }
    invisible(x)
}
