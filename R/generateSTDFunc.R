generateSTDFunc <- function(availableCriteria, criteria, managertype){
	func <- ""
	pattern <- "^prob.([^ .])"
	values <- c()
	numScr <- 0
	if(managertype == "TopN"){
		numScr <- 1
	}


	for(i in 1:length(criteria)){
		scr <- strsplit(criteria[i], split = " ", fixed = TRUE)
		
		switch(scr[[1]][1],
			fdrpass=, numpass={},
			prob.pt.fdr=, prob.pt.fam.fdr={
				if(length(grep("tvalue.std", values, fixed = TRUE, value = TRUE)) == 0){
					values <- c(values, "tvalue.std")
				}
				numScr <- numScr + 1
			},
			info={
				if(managertype == "TopN"){
					numScr <- numScr + 1
				}
			},
			prob.c=, prob.c.fdr={
				if(length(grep("czvalue", availableCriteria)) > 0){
					if(length(grep("czvalue", values, fixed = TRUE, value = TRUE)) == 0){
						values <- c(values, "czvalue.std")
					}
				}else if(length(grep("cvalue", availableCriteria)) > 0){
					if(length(grep("cvalue", values, fixed = TRUE, value = TRUE)) == 0){
						values <- c(values, "cvalue.std")
					}
				}
				numScr <- numScr + 1
			},
			{
				if(length(grep(pattern, scr[[1]][1])) > 0){
					tmpChar <- sub("prob.","",scr[[1]][1])
					tmp <- paste(tmpChar, "value.std", sep="")

					if(length(grep(paste(tmpChar,"value", sep=""), availableCriteria)) > 0){
						if(length(grep(tmp, values, fixed = TRUE, value = TRUE)) == 0){
							values <- c(values, tmp)
						}
					}
				}else{
					tmp <- paste(scr[[1]][1],".std",sep="")
					if(length(grep(tmp, values, fixed = TRUE, value = TRUE)) == 0){
						values <- c(values, tmp)
					}
				}
				numScr <- numScr + 1
			}
		)
	}
	
	if(numScr > 0){
		valueString <- ""
		func <- paste("( numPass / ",numScr," ) * ( ",
		if("info" %in% availableCriteria){"1 - info ) * ( "},
		paste(values, collapse = " + "),
		" )",sep="")
	}

	func
}
