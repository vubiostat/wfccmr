require(rpanel)
require(Wfccm)
source("topnpanel.R")
#rp.control("TopN")
#rp.tkrplot(1:100,cumsum(rnorm(100, 1))/1:100)a
#plot(1:100,cumsum(rnorm(100, 1))/1:100, type="l")
#Draw <- function(panel) {
#	with(panel, {
#		#plot(c(0,100),c(50,50), type="l")
#		plot(1:100,cumsum(rnorm(100, 1))/1:100, type="l")
#		abline(v=x)
#		#plot(c(x,x),c(0,10), type="l")
#		
#	})
#	panel	
#}

#panelD <- rp.control("Test!", x=50)

#rp.slider(panelD, x, 1, 100, Draw, "test", showvalue=TRUE, resolution=1)

raw <- read.wfccmdata("exampleData.txt")
data <- log2(raw)
info <- read.wfccminfo("exampleInfo.txt")
groups <- info$smoking.status
manager <- read.TopNManager("testtopn.txt")

for(i in (1:length(data))){
	#print(i)
	for(j in (1:length(data[[i]]))){
		#print(j)
		if (data[[i]][j] == "-Inf"){
			#print("zero")
			data[[i]][j] <- 0
		}
	}
}

final <- wfccmr(data, groups, tests=tests.wfccm(data, groups), manager)

topnpanel(final)
