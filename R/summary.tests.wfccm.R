summary.tests.wfccm <- function(..., file="", sep=",")
{
    cutoff <- function(x)
    {
        cut <- as.double(strsplit(format(x, scientific=TRUE), "e")[[1]][1])
        cutE <- 10 ** floor(log(x, 10))
        if (cut < 1.5)
            cut <- 1
        else if (cut < 2.25)
            cut <- 2
        else if (cut < 3.75)
            cut <- 2.5
        else if (cut < 7.5)
            cut <- 5
        else
            cut <- 10
        return(cut * cutE)
    }
    scores <- list(...)
    l <- length(scores)
    cols <- unique(unlist(lapply(scores, colnames)))
    grpnames <- names(scores)
    if (is.null(grpnames)) grpnames <- paste("Group", 1:l)
    cat("", grpnames, file=file, sep=sep, append=FALSE)
    if ("info" %in% cols)
    {
        table <- matrix(NA,1,l)
        rownames(table) <- "==0"
        for (i in 1:l)
        {
            if ("info" %in% colnames(scores[[i]]))
                table[1,i] <- sum(scores[[i]][,"info"] == 0)
        }
        cat("\n", "info", "\n", file=file, sep="", append=TRUE)
        write.table(table, file=file, sep=sep, col.names=FALSE, append=TRUE)
    }
    for (col in cols)
    {
        # columns we know to skip
        if (col == "info") next # info
        if (col == "sam") next # sam - use |sam|, aka asam
        if (col == "ranksum") next
        if (substr(col, nchar(col)-3, nchar(col)) == "rank") next # ranks
        if (substr(col, nchar(col)-4, nchar(col)) == "value") next # statistics that have a pvalue
        table <- matrix(NA,1,l)
        keys <- c()
        if (substr(col, 1, 5) == "prob.")
        {
            rownames(table) <- "Min"
            mode <- 1
            dec <- FALSE
            op <- "<"
        }
        else
        {
            rownames(table) <- "Max"
            mode <- 2
            dec <- TRUE
            op <- ">"
        }
        for (i in 1:l)
        {
            cold <- scores[[i]][,col]
            s <- sort(cold, decreasing=dec)
            table[1,i] <- s[1]
            if (mode == 1)
            {
                idx <- 10 ** (floor(log(s[1], 10)):-1)
                keys <- unique(c(-Inf, keys, idx, idx*5))
            }
            else if (mode == 2)
            {
                min300 <- s[min(300, length(s))]
                maxE <- 10 ** floor(log(s[1], 10))
                minE <- 10 ** floor(log(min300, 10))
                max1 <- round(s[1] / maxE, 0) * maxE
                min1 <- round(min300 / minE, 0) * minE
                range1 <- max1 - min1
                rangeE <- 10 ** floor(log(range1, 10))
                step1 <- round(range1 / 7 / rangeE, 2) * rangeE
                stepA <- cutoff(step1)
                keys <- unique(c(Inf, keys, seq(max1, min1, -stepA)))
            }
        }
        keys <- sort(keys, decreasing=dec)
        table2 <- matrix(0, length(keys), l)
        rownames(table2) <- paste(op, keys)
        for (i in 1:l)
        {
            if (!col %in% colnames(scores[[i]])) next
            table2[,i] <- apply(sapply(scores[[i]][,col], op, keys), 1, sum)
        }
        table2 <- as.matrix(table2[which(apply(diff(table2), 1, sum) > 0) + 1,])
        table <- rbind(table2, table)
        cat("\n", col, "\n", file=file, sep="", append=TRUE)
        write.table(table, file=file, sep=sep, col.names=FALSE, append=TRUE)
    }
}
