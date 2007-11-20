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
            return("there must be one name for each operator and vice versa")
        if (length(object@name) != length(object@values))
            return("there must be values for each name and vice versa")
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
    function(object)  cat("", as(object, "character"), "", sep="\n")
)

# Names
setMethod("names",
    signature(  x="Criteria"),
    function(x)  x@name
)

# Length
setMethod("length",
    signature(  x="Criteria"),
    function(x)  length(x@name)
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

# [
setMethod("[",
    signature(  x="Criteria",
                i="character",
                j="missing"),
    function(x, i, j, drop)  x[which(x@name %in% i)]
)
setMethod("[",
    signature(  x="Criteria",
                i="character",
                j="numeric"),
    function(x, i, j, drop)  x[which(x@name %in% i), j]
)

setMethod("[",
    signature(  x="Criteria",
                i="logical",
                j="missing"),
    function(x, i, j, drop)  x[which(i)]
)
setMethod("[",
    signature(  x="Criteria",
                i="logical",
                j="numeric"),
    function(x, i, j, drop)  x[which(i), j]
)

setMethod("[",
    signature(  x="Criteria",
                i="numeric",
                j="missing"),
    function(x, i, j, drop)  Criteria(x@name[i], x@operator[i], x@values[i])
)
setMethod("[",
    signature(  x="Criteria",
                i="numeric",
                j="numeric"),
    function(x, i, j, drop) #x[i][,j]
    {
        if (length(j) != length(i))
            stop("incorrect number of elements in 'i' and 'j'")
        Criteria(x@name[i], x@operator[i], mapply("[", x@values[i], j, SIMPLIFY=FALSE))
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
                i="missing",
                j="numeric"),
    function(x, i, j, drop)
    {
        if (length(j) != length(x))
            stop("incorrect number of elements in 'j'")
        Criteria(x@name, x@operator, mapply("[", x@values, j, SIMPLIFY=FALSE))
    }
)

# [<-
setReplaceMethod("[",
    signature(  x="Criteria",
                i="character",
                j="missing",
                value="Criteria"),
    function(x, i, j, value) 
    {
        x[which(x@name %in% i)] <- value
        x
    }
)
setReplaceMethod("[",
    signature(  x="Criteria",
                i="character",
                j="numeric",
                value="numeric"),
    function(x, i, j, value) 
    {
        x[which(x@name %in% i), j] <- value
        x
    }
)

setReplaceMethod("[",
    signature(  x="Criteria",
                i="logical",
                j="missing",
                value="Criteria"),
    function(x, i, j, value) 
    {
        x[which(i)] <- value
        x
    }
)
setReplaceMethod("[",
    signature(  x="Criteria",
                i="logical",
                j="numeric",
                value="numeric"),
    function(x, i, j, value) 
    {
        x[which(i), j] <- value
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
                i="numeric",
                j="numeric",
                value="numeric"),
    function(x, i, j, value)
    {
        if (length(j) != length(i))
            stop("incorrect number of elements in 'i' and 'j'")
        value <- rep(value, length.out=length(i))
        for (k in 1:length(i))
            x@values[[i[k]]][k[j]] <- value[k]
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
                i="missing",
                j="numeric",
                value="numeric"),
    function(x, i, j, value)
    {
        if (length(j) != length(x))
            stop("incorrect number of elements in 'j'")
        value <- rep(value, length.out=length(x))
        for (k in 1:length(x))
            x@values[[k]][k[j]] <- value[k]
        x
    }
)

# [[ -- Get combination n of criteria
setMethod("[[",
    signature(  x="Criteria",
                i="numeric",
                j="missing"),
    function(x, i, j)
    {
        base <- cumprod(c(1, sapply(x@values, length)))
        idx <- diff((i - 1) %% base) %/% base[-(length(x@values)+1)] + 1
        x[, idx]
    }
)
