sign.wfccm <- function(data, lead, rest)
{
    if (!is.data.frame(data))
        stop('Parameter data must be of class data.frame.')
    if (length(lead) > 1)
        stop('Parameter lead must be a single item.')
    if (!mode(lead) %in% c(mode(character()), mode(numeric())))
        stop('Parameter lead must be of mode character or numeric.')
    if (!mode(rest) %in% c(mode(character()), mode(numeric())))
        stop('Parameter rest must be of mode character or numeric.')
    if (mode(data[[lead]]) != mode(numeric()))
        stop(paste('Column \'', lead, '\' is not of mode numeric.', sep=''))

    signs <- sign(data[[lead]])
    #signs[signs == 0] <- 1
    for (col in rest)
    {
        if (mode(data[[col]]) != mode(numeric()))
            warning(paste('Skipped column \'', col, '\' because it is not of mode numeric', sep=''))
        else
            data[[col]] <- abs(data[[col]]) * signs
    }
    data
}
