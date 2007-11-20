setClass("CriteriaSet",
    representation( criteria="Criteria",
                    pass="character"),
    prototype(  criteria=Criteria(),
                pass=""),
    validity=function(object)
    {
        if (any(sapply(object@criteria@values, length) != 1))
            return("there can only be one cutoff value per criteria")
        if (length(object@pass) != 1)
            return("there can only be one pass string")
        TRUE
    }
)

# Constructor
CriteriaSet <- function(criteria=Criteria(), pass="")
{
    if (length(pass) > 1)
        warning("only the first element of pass is used, all others are ignored")
    new("CriteriaSet", criteria=as.Criteria(criteria), pass=as.character(pass)[1])
}

# Tests
is.CriteriaSet <- function(x)  is(x, "CriteriaSet")

# Coersion
setAs(from="CriteriaSet", to="character",
    function(from)  paste(paste(from@criteria, collapse="\n"), from@pass, sep="\n")
)
setMethod("as.character",
    signature(  x="CriteriaSet"),
    function(x)  as(x, "character")
)

# Show
setMethod("show",
    signature(  object="CriteriaSet"),
    function(object)
    {
        cat("", as(object@criteria, "character"), "",
            paste("Pass:", object@pass), "",
            sep="\n")
    }
)

# Names
setMethod("names",
    signature(  x="CriteriaSet"),
    function(x)  x@criteria@name
)
