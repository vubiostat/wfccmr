winners.wfccm <- function(stats, criteriaset) {
    f <- function(x) { with(stats, eval(parse(text=x))) }
    passers <- mapply(f, as.character(criteriaset@criteria))
    numPass <- rowSums(passers)
    fdrPass <- rowSums(passers[,grep("fdr$", colnames(passers), value=TRUE)])
    pass <- with(stats, eval(parse(text=criteriaset@pass)))
    return(data.frame(pass, fdrPass, numPass))
}
