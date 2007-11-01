winners.wfccm <- function(stats, criteriaset)
{
    f <- function(x)
        with(stats, eval(parse(text=x)))
    passers <- mapply(f, as.character(criteriaset@criteria))
    numPass <- apply(passers, 1, sum)
    fdrPass <- apply(passers[#TODO#,], 1, sum)
    fdrPass <- numPass <- rep(0, nrow(stats))
    for (crit in criteriaset@criteria)
    {
        if (!crit@name %in% colnames(stats)) next
        curPass <- with(stats, eval(parse(text=as.character(crit))))
        if (length(grep('adj$', crit@name)) > 0)
        {
            fdrPass <- fdrPass + curPass
        }
        numPass <- numPass + curPass
    }
    pass <- with(stats, eval(parse(text=criteriaset@pass)))
    return(data.frame(pass, fdrPass, numPass))
}
