setClass("CutoffManager",
    representation( numPass="Criteria",
                    fdrPass="Criteria"),
    prototype(  name="cutoff",
                numPass=Criteria("numPass",">=",1),
                fdrPass=Criteria("fdrPass",">=",1)),
    contains="CriteriaManager",
    validity=function(object) {
        if (length(object@numPass) > 1)
            return("there can only be one numPass criteria")
        if (length(object@fdrPass) > 1)
            return("there can only be one fdrPass criteria")
        if (length(object@name) != 1)
            return("there can only be one name")
        if (length(object@wfccmfunction) != 1)
            return("there can only be one WFCCM function")
        if (length(object@prefilter) != 1)
            return("there can only be one pre-filter function")
        if (any(object@criteria@operator %in% c("ASC", "DESC")))
            return("cannot use ASC or DESC operators for cutoff")
        TRUE
    }
)

# Constructor
CutoffManager <- function(criteria=Criteria(), name="cutoff", sign="", wfccmfunction="", prefilter="", permutations=10000, numPass=Criteria("numPass",">=",1), fdrPass=Criteria("fdrPass",">=",1)) {
    new("CutoffManager", criteria=criteria, name=name, sign=sign, wfccmfunction=wfccmfunction, prefilter=prefilter, permutations=permutations, numPass=numPass, fdrPass=fdrPass)
}

# Write
write.CutoffManager <- function(x, file) {
    cat(x@name, "",
        x@prefilter, "",
        x@wfccmfunction, "",
        paste(x@sign, collapse=" "), "",
        x@permutations, "",
        as(x@criteria, "character"),
        paste("numPass", x@numPass@operator, paste(x@numPass@values, collapse=" ")),
        paste("fdrPass", x@fdrPass@operator, paste(x@fdrPass@values, collapse=" ")),
        file=file, sep="\n")
}

# Read
read.CutoffManager <- function(file) {
    args <- read.CriteriaManager(file)
    args$numPass <- args$criteria["numPass"]
    args$fdrPass <- args$criteria["fdrPass"]
    args$criteria <- args$criteria[! args$criteria@name %in% c("numPass","fdrPass")]
    do.call(CutoffManager, args)
}

# Tests
is.CutoffManager <- function(x) { is(x, "CutoffManager") }

# Coersion
setAs(from="CutoffManager", to="character",
    function(from) {
        paste(from@name,
            from@prefilter,
            from@wfccmfunction,
            paste(from@sign, collapse=" "),
            from@permutations,
            paste(from@criteria, collapse="\n"),
            paste("numPass", from@numPass@operator, paste(from@numPass@values, collapse=" ")),
            paste("fdrPass", from@fdrPass@operator, paste(from@fdrPass@values, collapse=" ")),
            sep="\n")
    }
)

# Print
setMethod("as.character",
    signature(  x="CutoffManager"),
    function(x)  as(x, "character")
)

# Show
setMethod("show",
    signature(  object="CutoffManager"),
    function(object) {
        cat("", paste("Criteria", object@name), "",
            paste("Pre-filter:", object@prefilter), "",
            paste("Function:", object@wfccmfunction), "",
            paste("Sign:", paste(object@sign, collapse=" ")), "",
            paste("Distance Permutations:", object@permutations), "",
            "Criteria:",
            as(object@criteria, "character"),
            paste("numPass",object@numPass@operator, paste(object@numPass@values, collapse=" ")),
            paste("fdrPass",object@numPass@operator, paste(object@fdrPass@values, collapse=" ")),
            "",
            sep="\n")
    }
)

# Get combinations
setMethod("length",
    signature(  x="CutoffManager"),
    function(x) {
        if (length(x@criteria@values) == 0)
            0
        else
            prod(sapply(x@criteria@values, length)) * length(x@numPass) * length(x@fdrPass)
    }
)

# Get CriteriaSet X (1-based)
setMethod("[[",
    signature(  x="CutoffManager",
                i="numeric",
                j="missing"),
    function(x, i, j) {
        n <- (i - 1) %/% prod(sapply(x@criteria@values, length))
        pass <-
            if (length(x@numPass) > 0)
                paste("fdrPass", x@fdrPass@operator, x@fdrPass@values[n %% length(x@fdrPass) + 1])
            else
                "fdrPass >= 1"
        n <- n %/% length(x@fdrPass)
        pass <- paste(pass, "&",
            if (length(x@numPass) > 0)
                paste("numPass", x@numPass@operator, x@numPass@values[n %% length(x@numPass) + 1])
            else
                "numPass >= 1"
            )
        CriteriaSet(x@criteria[[i]], pass)
    }
)
