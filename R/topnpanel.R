topnpanel <- function(object){
    Draw <- function(panel) {
        with(panel, {
            plot(object, show.legend=FALSE)
            abline(v=x, lwd=2, col=3)
            #text(x = 22, y = 25, labels = paste(names(object[[1]]$training$accuracy[1]),": ",round(object[[x]]$training$accuracy[[1]]*100, digits = 1)))
            #text(x = 22, y = 20, labels = paste(names(object[[1]]$training$accuracy[2]),": ",round(object[[x]]$training$accuracy[[2]]*100, digits = 1)))
            #text(x = 22, y = 15, labels = paste(names(object[[1]]$training$accuracy[3]),": ",round(object[[x]]$training$accuracy[[3]]*100, digits = 1)))
        })
        return(panel)
    }

    x <- 0
    panel <- rp.control("TopN")
    rp.slider(panel, x, 1, length(object), Draw, showvalue=TRUE, resolution=1)
}
