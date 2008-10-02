tests.wfccm <- function(x, grp, tests = c("t", "ks", "wilcoxon", "sam", "wga", "huwright", "info"), ...) {
    if (nlevels(factor(grp)) != 2)
        stop("There must be 2 groups for statistical tests.")
    namemap <- c(t="t", ks="d", wilcoxon="c", fisher="f")
    tests <- match.arg(tests, several.ok=TRUE)
    n <- nrow(x)
    v <- ncol(x)
    d <- split(x, grp)
    result <- data.frame(matrix(0, v, 0), row.names=colnames(x))
    for (test in tests) {
        dat <- list()

        if (test %in% c("t", "ks", "wilcoxon", "wilcox")) {
            z <- test == "wilcoxon"
            func <- get(paste(test, "test", sep="."))
            dat <- data.frame(t(mapply(function(x,y) {
                tmp <- func(x,y)
                if (z)
                    return(unlist(tmp[c("statistic","p.value","z")]))
                else
                    return(unlist(tmp[c("statistic","p.value")]))
            }, d[[1]], d[[2]])))
            name <- namemap[test]
            names(dat) <- c(paste(name, "value", sep="."), paste("prob", name, sep="."))
            if (z)
                names(dat)[3] <- "cz.value"
            fdr <- fdrAdjustment(dat[[2]])
            names(fdr) <- paste(names(dat)[2], c("rank", "fdr"), sep=".")
            dat <- cbind(dat, fdr)
        }

        if (test == "fisher") {
            dat <- data.frame(t(mapply(function(x,y) {
                return(unlist(fisher.test(table(x > 0, y > 0), ...)[c("estimate","p.value")]))
            }, d[[1]], d[[2]])))
            names(dat) <- c("f.value", "prob.f")
            fdr <- fdrAdjustment(dat[[2]])
            names(fdr) <- paste(names(dat)[2], c("rank", "fdr"), sep=".")
            dat <- cbind(dat, fdr)
        }

        if (test %in% c("wga")) {
            func <- get(paste(test, "test", sep="."))
            dat[[test]] <- mapply(func, d[[1]], d[[2]])
            dat[[paste(test, "rank", sep=".")]] <- rank(-dat[[1]])
        }

        if (test == "sam") {
            if (require(samr)) {
                tmp <- as.vector(samr(list(x=t(x), y=grp), "Quantitative", nperms=1, ...)$tt)
                dat$sam <- tmp
                dat$asam <- abs(tmp)
                dat$asam.rank <- rank(-dat$asam)
            }
        }

        if (test == "huwright") {
            dat$huwright <- huwright.test(d[[1]], d[[2]])
            dat$huwright.rank <- rank(-dat$huwright)
        }

        if (test == "info") {
            dat$info <- mapply(info.test, d[[1]], d[[2]])
        }

        result <- cbind(result, dat)
    }

    result <- cbind(result, rank.wfccm(result[,grep(".rank$", colnames(result), value=TRUE)], ties.break=1)[c("ranksum","rank")])
    
    return(structure(result, class=c("tests.wfccm", "data.frame")))
}
