winners.wfccm <- function(stats, criteriaset)
{
    f <- function(x)
        with(stats, eval(parse(text=x)))
    passers <- mapply(f, as.character(criteriaset@criteria))
    numPass <- apply(passers, 1, sum)
    fdrPass <- apply(passers[,grep('fdr$', colnames(stats))], 1, sum)
    #fdrPass <- numPass <- numeric(nrow(stats))
    #for (crit in criteriaset@criteria)
    #{
    #    if (!crit@name %in% colnames(stats)) next
    #    curPass <- with(stats, eval(parse(text=as.character(crit))))
    #    if (length(grep('fdr$', crit@name)) > 0)
    #    {
    #        fdrPass <- fdrPass + curPass
    #    }
    #    numPass <- numPass + curPass
    #}
    pass <- with(stats, eval(parse(text=criteriaset@pass)))
    data.frame(pass, fdrPass, numPass)
}
