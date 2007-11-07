setClass("Criteria",
    representation( name="character",
                    operator="character",
                    values="list"),
    prototype(  name=character(),
                operator=character(),
                values=list()),
    validity=function(object)
    {
        rankcols <- object@operator %in% c("ASC", "DESC")
        cutcols <- which(!rankcols)
        rankcols <- which(rankcols)
        if (length(object@name) != length(object@operator))
            return("there must be one name for each operator and vice versa"
        if (length(object@name) != length(object@values))
            return("there must be values for each name and vice versa"
        if (!all(sapply(object@values, is.numeric)))
            return("cutoff values must be numeric")
        if(any(duplicated(paste(object@name, object@operator))))
            return("combine duplicate names")
        if ((length(object@values[cutcols]) > 0) &&
            any(is.na(unlist(object@values[cutcols]))))
            return("NA is not a valid cutoff for operators")
        if (!all(object@operator %in% c(">", ">=", "<", "<=", "!=", "==", "ASC", "DESC")))
            return("operator is invalid")
        if ((length(object@values[rankcols]) > 0) &&
            (   any(sapply(object@values[rankcols], length) != 1) ||
                !all(is.na(unlist(object@values[rankcols])))
            ))
            return("ASC and DESC must have a single NA cutoff")
        TRUE
    }
)

# Constructor
Criteria <- function(name=character(), operator=character(), values=list())
{
    if (!is.list(values))
    {
        if (length(name) == 1)
            values <- list(values)
        else
            values <- as.list(values)
    }
    len <- length(name)
    operator <- rep(operator, length.out=len)
    values <- rep(values, length.out=len)
    new("Criteria", name=as.character(name), operator=as.character(operator), values=lapply(values, as.numeric))
}

# Tests
is.Criteria <- function(x)  is(x, "Criteria")

# Coersion
# Criteria as.character
setAs(from="Criteria", to="character",
    function(from)
    {
        result <- paste(from@name, from@operator, sapply(from@values, paste, collapse=" "))
        names(result) <- from@name
        result
    }
)
setMethod("as.character",
    signature(  x="Criteria"),
    function(x)  as(x, "character")
)
# character as.Criteria
setGeneric("as.Criteria",
    function(x)  standardGeneric("as.Criteria")
)
setAs(from="character", to="Criteria",
    function(from)
    {
        # get a single space around the operator
        from <- sub("([[:space:]]*(>=?|<=?|!=|==)[[:space:]]*)", " \\1 ", from)
        # make multiple spaces a single
        from <- gsub("[[:space:]]+", " ", from)
        # split on spaces
        split <- strsplit(from, " ")
        name <- sapply(split, "[", 1)
        oper <- sapply(split, "[", 2)
        valus <- sapply(sapply(split, "[", -c(1,2)), as.numeric)
        Criteria(name, oper, valus)
    }
)
setMethod("as.Criteria",
    signature(  x="ANY"),
    function(x)  as(x, "Criteria")
)

# Show
setMethod("show",
    signature(  object="Criteria"),
    function(object)  print(as(object, "character"))
)

# Names
setMethod("names",
    signature(  x="Criteria"),
    function(x)  x@name
)
setReplaceMethod("names",
    signature(  x="Criteria",
                value="character"),
    function(x, value)
    {
        x@name <- rep(c(value, NA), length.out=length(x@name))
        x
    }
)

# Length
setMethod("length",
    signature(  x="Criteria"),
    function(x)  length(x@name)
)
setReplaceMethod("length",
    signature(  x="Criteria",
                value="ANY"),
    function(x, value)  stop("operation not supported.")
)

# Concatenate
setMethod("c",
    signature(  x="Criteria",
                recursive="missing"),
    function(x, ..., recursive)
    {
        l <- list(...)
        if (length(l) > 0)
        {
            l <- sapply(l[sapply(l, canCoerce, "Criteria")], as.Criteria)
            name <- sapply(l, function(x) x@name)
            oper <- sapply(l, function(x) x@operator)
            valus <- sapply(l, function(x) x@values)
            Criteria(c(x@name, name), c(x@operator, oper), c(x@values, valus))
        }
        else
            x
    }
)

# Vector
setMethod("[",
    signature(  x="Criteria",
                i="character"),
    function(x, i, j, drop)
    {
        idx <- which(x@name %in% i)
        if (missing(j))
            x[idx]
        else
            x[idx, j]
    }
)
setMethod("[",
    signature(  x="Criteria",
                i="missing"),
    function(x, i, j, drop)
    {
        if (length(j) < length(x))
            stop("not enough elements in j")
        if (length(j) > length(x))
            j <- j[1:length(x)]
        Criteria(x@name, x@operator, mapply("[", x@values, j, SIMPLIFY=FALSE))
    }
)
setMethod("[",
    signature(  x="Criteria",
                i="numeric",
                j="missing"),
    function(x, i, j, drop)
    {
        Criteria(x@name[i], x@operator[i], x@values[i])
    }
)
setMethod("[",
    signature(  x="Criteria",
                i="missing",
                j="missing"),
    function(x, i, j, drop)  x
)
setMethod("[",
    signature(  x="Criteria",
                i="numeric"),
    function(x, i, j, drop)
    {
        if (length((1:length(x))[i]) == 1 && !missing(j))
        {
                Criteria(x@name[i], x@operator[i], x@values[[i]][j])
        }
        else
        {
            if (!missing(j))
                warning("second argument to extractor function ignored")
            x[i]
        }
    }
)
setReplaceMethod("[",
    signature(  x="Criteria",
                i="character",
                value="Criteria"),
    function(x, i, j, value)
    {
        idx <- which(x@name %in% i)
        if (missing(j))
            x[idx] <- value
        else
            x[idx, j] <- value
        x
    }
)
setReplaceMethod("[",
    signature(  x="Criteria",
                i="numeric",
                j="missing",
                value="Criteria"),
    function(x, i, j, value)
    {
        x@name[i] <- value@name
        x@operator[i] <- value@operator
        x@values[i] <- value@values
        x
    }
)
setReplaceMethod("[",
    signature(  x="Criteria",
                i="missing",
                j="missing",
                value="Criteria"),
    function(x, i, j, value)
    {
        x@name[] <- value@name
        x@operator[] <- value@operator
        x@values[] <- value@values
        x
    }
)
setReplaceMethod("[",
    signature(  x="Criteria",
                i="numeric",
                value="Criteria"),
    function(x, i, j, value)
    {
        if (length((1:length(x))[i]) == 1 && !missing(j))
                x@values[[i]][j] <- value@values
        else
        {
            if (!missing(j))
                warning("second argument to replacement function ignored")
            x[i] <- value
        }
        x
    }
)
setReplaceMethod("[",
    signature(  x="Criteria"),
    function(x, i, j, value)
    {
        if (missing(i) && missing(j))
            x[] <- as.Criteria(value)
        else if (missing(j))
            x[i] <- as.Criteria(value)
        else
            x[i,j] <- as.Criteria(value)
        x
    }
)

# Get combination X (1-based) of criteria
setMethod("[[",
    signature(  x="Criteria",
                i="numeric",
                j="missing"),
    function(x, i, j)
    {
        base <- cumprod(c(1, sapply(x@values, length)))
        idx <- diff((i - 1) %% base) %/% base[-length(x@values)] + 1
        x[, idx]
    }
)
