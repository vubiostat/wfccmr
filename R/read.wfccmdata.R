read.wfccmdata <- function(file, sep = "\t", ...) {
    data <- read.table(file, header=TRUE, sep, na.strings=c(".", "NA", "NaN"), row.names=1, comment.char="", strip.white=TRUE, ...)
    data <- data[,colnames(data) != "id"]
    return(data.frame(t(data)))
}
