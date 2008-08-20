setClass("TopNManager",
    representation( topN="numeric"),
    prototype(  name="top200",
                topN=200),
    contains="CriteriaManager",
    validity=function(object) {
        if (length(object@name) != 1)
            return("there can only be one name")
        if (length(object@wfccmfunction) != 1)
            return("there can only be one WFCCM function")
        if (length(object@prefilter) != 1)
            return("there can only be one pre-filter function")
        if (length(object@topN) != 1)
            return("there can only be one topN")
        TRUE
    }
)

# Constructor
TopNManager <- function(criteria=Criteria(), name=paste("top",topN,sep=""), sign="", wfccmfunction="", prefilter="", permutations=10000, topN=100)
new("TopNManager", criteria=criteria, name=name, sign=sign, wfccmfunction=wfccmfunction, prefilter=prefilter, permutations=permutations, topN=topN)

# Write
write.TopNManager <- function(x, file) {
    cat(paste("top", x@topN, sep=""), "",
        x@prefilter, "",
        x@wfccmfunction, "",
        paste(x@sign, collapse=" "), "",
        x@permutations, "",
        as(x@criteria, "character"),
        file=file, sep="\n")
}

# Read
read.TopNManager <- function(file) {
    args <- read.CriteriaManager(file)
    args$topN <- as.numeric(gsub("[^0-9]", "", args$name))
    do.call(TopNManager, args)
}

# Tests
is.TopNManager <- function(x)  is(x, "TopNManager")

# Coersion
setAs(from="TopNManager", to="character",
    function(from) {
        paste(paste("top", from@topN, sep=""),
            from@prefilter,
            from@wfccmfunction,
            paste(from@sign, collapse=" "),
            from@permutations,
            paste(from@criteria, collapse="\n"),
            sep="\n")
    }
)

# Print
setMethod("as.character",
    signature(  x="TopNManager"),
    function(x)  as(x, "character")
)

# Show
setMethod("show",
    signature(  object="TopNManager"),
    function(object) {
        cat("", paste("Criteria", paste("top", object@topN, sep="")), "",
            paste("Pre-filter:", object@prefilter), "",
            paste("Function:", object@wfccmfunction), "",
            paste("Sign:", paste(object@sign, collapse=" ")), "",
            paste("Distance Permutations:", object@permutations), "",
            "Criteria:",
            as(object@criteria, "character"),
            "",
            sep="\n")
    }
)

# Get combinations
setMethod("length",
    signature(  x="TopNManager"),
    function(x)  x@topN
)
setReplaceMethod("length",
    signature(  x="TopNManager",
                value="numeric"),
    function(x, value) {
        x@topN <- value
        x
    }
)

# Get CriteriaSet X (1-based)
setMethod("[[",
    signature(  x="TopNManager",
                i="numeric",
                j="missing"),
    function(x, i, j) {
        n <- (i - 1) %% x@topN
        CriteriaSet(x@criteria[[i]], paste("(rank <=", n, ") & (numPass >= 1)"))
    }
)
