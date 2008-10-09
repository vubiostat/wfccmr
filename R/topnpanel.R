topnpanel <- function(final){
	require(rpanel)
	Draw <- function(panel) {
		with(panel, {
			plot(final)
			abline(v=x, lwd=2, col=3)
			text(x = 22, y = 25, labels = paste(names(final[[1]]$training$accuracy[1]),": ",round(final[[x]]$training$accuracy[[1]]*100, digits = 1)))
			text(x = 22, y = 20, labels = paste(names(final[[1]]$training$accuracy[2]),": ",round(final[[x]]$training$accuracy[[2]]*100, digits = 1)))
			text(x = 22, y = 15, labels = paste(names(final[[1]]$training$accuracy[3]),": ",round(final[[x]]$training$accuracy[[3]]*100, digits = 1)))
		})
		panel
	}

	None <- function(panel) {
		#Don't do anything!
	}
	class(final) <- c("topn", "list")
	x <- 0
	overall <- 0
	group1 <- 0
	group2 <- 0

	panel <- rp.control("TopN")
	rp.slider(panel, x, 1, 30, Draw, showvalue=TRUE, resolution=1)
}
