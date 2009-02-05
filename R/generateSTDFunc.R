generateSTDfunc <- function(criteria, availableCriteria) {
    func <- ""
    pattern <- "^prob\\.([^ .])"
    values <- c()
    numScr <- 0
    criteria <- strsplit(as.character(criteria), split=" ", fixed=TRUE)

    for (scr in criteria) {
        switch(scr[1],
            info={}, fdrPass={}, numPass={},
            prob.pt.fdr=, prob.pt.fam.fdr={
                if (length(grep("tvalue.std", values, fixed=TRUE, value=TRUE)) == 0)
                    values <- c(values, "tvalue.std")
                numScr <- numScr + 1
            },
            prob.c=, prob.c.fdr={
                if (length(grep("cz.value", availableCriteria)) > 0) {
                    if (length(grep("cz.value", values, fixed=TRUE, value=TRUE)) == 0)
                        values <- c(values, "cz.value.std")
                } else if (length(grep("c.value", availableCriteria)) > 0) {
                    if (length(grep("c.value", values, fixed=TRUE, value=TRUE)) == 0)
                        values <- c(values, "c.value.std")
                }
                numScr <- numScr + 1
            },
            {
                if (length(grep(pattern, scr[1])) > 0) {
                    tmpChar <- sub("prob.", "", scr[1])
                    tmp <- paste(tmpChar, "value.std", sep=".")

                    if (length(grep(paste(tmpChar, "value", sep="."), availableCriteria)) > 0) {
                        if (length(grep(tmp, values, fixed=TRUE, value=TRUE)) == 0)
                            values <- c(values, tmp)
                    }
                } else {
                    tmp <- paste(scr[1], "std", sep=".")
                    if (length(grep(tmp, values, fixed=TRUE, value=TRUE)) == 0)
                        values <- c(values, tmp)
                }
                numScr <- numScr + 1
            }
        )
    }

    func <- paste(  if (numScr > 0) { paste("numPass / ", numScr, sep="") },
                    if ("info" %in% availableCriteria) { "1 - info" },
                    if (length(values) > 0) { paste(values, collapse=' + ') },
                    sep=") * (")

    if (nchar(func) > 0) {
        func <- paste("(", func, ")", sep="")
    }
    return(func)
}
