winners.wfccm <- function(stats, criteriaset) {
    f <- function(x) { with(stats, eval(parse(text=x))) }
    passers <- mapply(f, as.character(criteriaset@criteria))
    numPass <- rowSums(passers)
    fdrPass <- rowSums(passers[,grep("fdr$", colnames(stats), value=TRUE)])
    #fdrPass <- numPass <- numeric(nrow(stats))
    #for (crit in criteriaset@criteria)
    #{
    #    if (!crit@name %in% colnames(stats)) next
    #    curPass <- with(stats, eval(parse(text=as.character(crit))))
    #    if (length(grep("fdr$", crit@name)) > 0)
    #    {
    #        fdrPass <- fdrPass + curPass
    #    }
    #    numPass <- numPass + curPass
    #}
    pass <- with(stats, eval(parse(text=criteriaset@pass)))
    return(data.frame(pass, fdrPass, numPass))
}
