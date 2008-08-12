tests.wfccm <- function(x, grp, tests = c("t", "ks", "wilcoxon", "sam", "wga", "huwright", "info"), ...) {
    if (nlevels(factor(grp)) != 2)
        stop("There must be 2 groups for statistical tests.")
    namemap <- c(t="t", ks="d", wilcoxon="c", fisher="f")
    tests <- match.arg(tests, several.ok=TRUE)
    n <- dim(x)[1]
    v <- dim(x)[2]
    d <- split(x, grp)
    result <- data.frame(matrix(0, v, 0))
    for (test in tests) {
        dat <- list()
        if (test %in% c("t", "ks", "wilcoxon")) {
            dat <- list(c(), c())
            if (test == "wilcoxon")
                dat <- list(c(), c(), c())
            funcname <- paste(test, "test", sep=".")
            for (i in 1:v) {
                tmp <- do.call(funcname, list(d[[1]][,i], d[[2]][,i], ...))
                dat[[1]][i] <- tmp$statistic
                dat[[2]][i] <- tmp$p.value
                if (test == "wilcoxon")
                    dat[[3]][i] <- tmp$z
            }
            name <- namemap[test]
            names(dat) <- c(paste(name, "value", sep="."), paste("prob", name, sep="."))
            if (test == "wilcoxon")
                names(dat)[3] <- "cz.value"
            fdr <- fdrAdjustment(dat[[2]])
            names(fdr) <- paste(names(dat)[2], c("rank", "fdr"), sep=".")
            dat <- cbind(dat, fdr)
        }
        if (test == "fisher") {
            for (i in 1:v) {
                tmp <- fisher.test(table(d[[1]][,i] > 0, d[[2]][,i] > 0), ...)
                dat$f.value[i] <- tmp$estimate
                dat$prob.f[i] <- tmp$p.value
            }
            fdr <- fdrAdjustment(dat[[2]])
            names(fdr) <- paste(names(dat)[2], c("rank", "fdr"), sep=".")
            dat <- cbind(dat, fdr)
        }
        if (test %in% c("wga")) {
            dat <- list(c())
            funcname <- paste(test, "test", sep=".")
            for (i in 1:v) {
                dat[[1]][i] <- do.call(funcname, list(d[[1]][,i], d[[2]][,i]))
            }
            names(dat) <- test
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
            dat$huwright <- huwright.test(d[[1]],d[[2]])
            dat$huwright.rank <- rank(-dat$huwright)
        }
        if (test == "info") {
            for (i in 1:v) {
                dat$info[i] <- info.test(d[[1]][,i], d[[2]][,i])
            }
        }
        result <- cbind(result, dat)
    }
    result <- cbind(result, rank.wfccm(result[,colnames(result)[grep(".rank$", colnames(result))]], ties.break=1)[c("ranksum","rank")])
    class(result) <- c("tests.wfccm", "data.frame")
    result
}
