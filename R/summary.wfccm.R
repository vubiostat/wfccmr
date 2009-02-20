summary.wfccm <- function(object, ...) {
    digits <- list(...)$digits
    if (is.null(digits)) digits <- 1
    f1 <- function(x, a, b) { x[[a]][[b]] }
    f2 <- function(x, a, b) { sum(x[[a]][[b]]) }
    winners <- sapply(object, f2, "winners", "pass")

    acc <- round(100 * t(sapply(object, f1, "training", "accuracy")), digits)
    cnt <- t(sapply(object, f1, "training", "n"))
    colnames(cnt) <- paste(colnames(cnt), "n", sep=".")
    distance.p <- sapply(object, f1, "training", "distancep")
    table <- cbind(acc, cnt)
    x <- seq(2, ncol(table), 2)
    result <- cbind(winners, subset(table, select=order(c(x-1, x))), distance.p)

    acc <- sapply(object, f1, "testing", "accuracy")
    if (!all(sapply(acc, is.null))) {
        cnt <- t(sapply(object, f1, "testing", "n"))
        colnames(cnt) <- paste(colnames(cnt), "n", sep=".")
        distance.p.test <- sapply(object, f1, "testing", "distancep")
        table <- cbind(round(100 * t(acc), digits), cnt)
        x <- seq(2, ncol(table), 2)
        colnames(result) <- paste(colnames(result), "train", sep=".")
        colnames(table) <- paste(colnames(table), "test", sep=".")
        result <- cbind(result, subset(table, select=order(c(x-1, x))), distance.p.test)
    }

    return(structure(data.frame(result), class=c("summary.wfccm", "data.frame")))
}
