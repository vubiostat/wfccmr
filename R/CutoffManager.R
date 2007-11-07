setClass("CutoffManager",
    representation( numPass="numeric",
                    fdrPass="numeric"),
    prototype(  name="cutoff",
                numPass=1,
                fdrPass=1),
    contains="CriteriaManager",
    validity=function(object)
    {
        if (length(object@name) != 1)
            return("there can only be one name")
        if (length(object@wfccmfunction) != 1)
            return("there can only be one WFCCM function")
        if (length(object@prefilter) != 1)
            return("there can only be one pre-filter function")
        rankcols <- object@criteria@operator %in% c("ASC", "DESC")
        if (any(rankcols))
            return("cannot use ASC or DESC operators for cutoff")
        TRUE
    }
)

# Constructor
CutoffManager <- function(criteria=Criteria(), name="cutoff", sign="", wfccmfunction="", prefilter="", permutations=10000, numPass=1, fdrPass=1)
new("CutoffManager", criteria=criteria, name=name, sign=sign, wfccmfunction=wfccmfunction, prefilter=prefilter, permutations=permutations, numPass=numPass, fdrPass=fdrPass)

# Write
write.CutoffManager <- function(x, file)
{
    cat(x@name, "",
        x@prefilter, "",
        x@wfccmfunction, "",
        paste(x@sign, collapse=" "), "",
        x@permutations, "",
        paste(x@criteria, collapse="\n"),
        paste("numPass", ">=", paste(x@numPass, collapse=", ")),
        paste("fdrPass", ">=", paste(x@fdrPass, collapse=", ")),
        file=file, sep="\n")
}

# Tests
is.CutoffManager <- function(x)  is(x, "CutoffManager")

# Coersion
setAs(from="CutoffManager", to="character",
    function(from)
    {
        paste(from@name,
            from@prefilter,
            from@wfccmfunction,
            paste(from@sign, collapse=" "),
            from@permutations,
            paste(from@criteria, collapse="\n"),
            paste("numPass", ">=", paste(from@numPass, collapse=", ")),
            paste("fdrPass", ">=", paste(from@fdrPass, collapse=", ")),
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
    function(object)
    {
        cat(paste("", paste("Criteria", object@name), "",
            paste("Pre-filter:", object@prefilter), "",
            paste("Function:", object@wfccmfunction), "",
            paste("Sign:", paste(object@sign, collapse=" ")), "",
            paste("Distance Permutations:", object@permutations), "",
            "Criteria:",
            paste(as(object@criteria, "character"), collapse="\n"),
            paste("numPass",">=", paste(object@numPass, collapse=", ")),
            paste("fdrPass",">=", paste(object@fdrPass, collapse=", ")),
            "", "",
            sep="\n"))
    }
)

# Get combinations
setMethod("length",
    signature(  x="CutoffManager"),
    function(x)
    {
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
    function(x, i, j)
    {
        n <- (i - 1) %/% prod(sapply(x@criteria@values, length))
        pass <-
            if (length(x@numPass) > 0)
                paste("fdrPass >=", x@fdrPass[n %% length(x@fdrPass) + 1])
            else
                "fdrPass >= 1"
        n <- n %/% length(x@fdrPass)
        pass <- paste(pass, "&",
            if (length(x@numPass) > 0)
                paste("numPass >=", x@numPass[n %% length(x@numPass) + 1])
            else
                "numPass >= 1"
            )
        CriteriaSet(x@criteria[[i]], pass)
    }
)
