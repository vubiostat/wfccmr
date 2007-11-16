read.wfccminfo <- function(file, sep="\t", ...)
{
    return(read.table(file, header=TRUE, sep, na.strings=c(".", "", "NA", "NaN"), row.names=1, comment.char="", strip.white=TRUE, ...))
}
