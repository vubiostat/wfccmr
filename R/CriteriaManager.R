setClass('CriteriaManager',
    representation( criteria='Criteria',
                    name='character',
                    sign='character',
                    wfccmfunction='character',
                    prefilter='character',
                    permutations='numeric',
                    'VIRTUAL'),
    prototype(  criteria=Criteria(),
                name=character(),
                sign=character(),
                wfccmfunction=character(),
                prefilter=character(),
                permutations=0),
    validity=function(object)
    {
        if (length(object@name) != 1)
            return('there can only be one name')
        if (length(object@wfccmfunction) != 1)
            return('there can only be one WFCCM function')
        if (length(object@prefilter) != 1)
            return('there can only be one pre-filter function')
        if (length(object@permutations) != 1)
            return('there can only be one distance permutations number')
        TRUE
    }
)

# Constructor
CriteriaManager <- function(criteria=Criteria(), name=character(), sign=character(), wfccmfunction=character(), prefilter=character(), permutations=0)
{
    new('CriteriaManager', criteria=criteria, name=name, sign=sign, wfccmfunction=wfccmfunction, prefilter=prefilter, permutations=permutations)
}

# Write
write.CriteriaManager <- function(x, file)
{
    cat(x@name, '',
        x@prefilter, '',
        x@wfccmfunction, '',
        paste(x@sign, collapse=' '), '',
        x@permutations, '',
        paste(x@criteria, collapse='\n'),
        file=file, sep='\n')
}

# Read from file
read.CriteriaManager <- function(file)
{
    data <- scan(file, what=character(0), sep='\n', blank.lines.skip=FALSE, quiet=TRUE)
    fix <- function(x)
    {
        gsub('value', '.value', gsub('_', '.', x))
    }
    name <- data[1]
    prefilter <- fix(data[3])
    wfccmfunction <- fix(data[5])
    sign <- fix(strsplit(data[7], ' ')[[1]])
    old.option <- options(warn = -1)
    on.exit(options(old.option))
    permutations <- as.numeric(data[9])
    next.line <- 11
    if (is.na(permutations))
    {
        permutations <- 0
        next.line <- 9
    }
    criteria <- Criteria()
    for (line in next.line:length(data))
    {
        if (nchar(data[line]) == 0) break
        criteria[line - next.line + 1] <- as.Criteria(data[line])
    }
    CriteriaManager(criteria=criteria, name=name, prefilter=prefilter, wfccmfunction=wfccmfunction, sign=sign, permutations=permutations)
}

# Tests
is.CriteriaManager <- function(x)
{
    is(x, 'CriteriaManager')
}

# Coersion
setAs(from='CriteriaManager', to='character',
    function(from)
    {
        paste(x@name,
            x@prefilter,
            x@wfccmfunction,
            paste(x@sign, collapse=' '),
            x@permutations,
            paste(x@criteria, collapse='\n'),
            sep='\n')
    }
)
setMethod('as.character',
    signature(  x='CriteriaManager'),
    function(x)
    {
        as(x, 'character')
    }
)

# Show
setMethod('show',
    signature(  object='CriteriaManager'),
    function(object)
    {
        print(paste('', paste('Criteria', from@name), '',
            paste('Pre-filter:', from@prefilter), '',
            paste('Function:', from@wfccmfunction), '',
            paste('Sign:', paste(from@sign, collapse=' ')), '',
            paste('Distance Permutations:', from@permutations), '',
            'Criteria:',
            paste(lapply(from@criteria, as, 'character'), collapse='\n'),
            sep='\n'))
    }
)

# Names
setMethod('names',
    signature(  x='CriteriaManager'),
    function(x)
    {
        x@name
    }
)
setReplaceMethod('names',
    signature(  x='CriteriaManager',
                value='character'),
    function(x, value)
    {
        x@name <- value
        x
    }
)

# Length
setMethod('length',
    signature(  x='CriteriaManager'),
    function(x)
    {
        stop('operation not supported.')
    }
)
setReplaceMethod('length',
    signature(  x='CriteriaManager'),
    function(x, value)
    {
        stop('operation not supported.')
    }
)

# Get CriteriaSet X (1-based)
setMethod('[[',
    signature(  x='CriteriaManager'),
    function(x, i, j)
    {
        stop('operation not supported.')
    }
)
