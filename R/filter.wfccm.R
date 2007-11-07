filter.wfccm <- function(data, func)
{
    if (class(data) != class(data.frame()))
        stop('Parameter data must be of class data.frame.')
    if (mode(func) != mode(character()) || length(func) > 1)
        stop('Parameter func must be a single character string.')
    
    data[which(with(data, eval(parse(text=func)))),]
}
