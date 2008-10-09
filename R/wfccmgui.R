wfccmgui <- function(availableCriteria = c("asam","asam.rank","cvalue","dvalue","huwright",
				"huwright.rank","info","prob.c","prob.c.fdr","prob.c.rank","prob.d",
				"prob.d.fdr","prob.d.rank","prob.t","prob.t.fdr","prob.t.rank","sam",
				"tvalue","wga","wga.rank")){
local({
	require(tcltk2) || stop("tcltk2 support is absent. Install the tcltk2 package to continue")
	#require(gdata)
	main <- function() {
###FUNCTIONS
		addcriteria <- function(crit, oper, val) {
			if (tclvalue(oper) == "DESC" || tclvalue(oper) == "ASC") {
				critstring <- paste(tclvalue(crit),tclvalue(oper))
			} else {
				critstring <- paste(tclvalue(crit),tclvalue(oper), tclvalue(val))
			}
			tkinsert(criterialb,"end", critstring)
				value <<- tclVar("")
				tkconfigure(entry.value, textvariable=value)
		}

	removecriteria <- function() {
		tkdelete(criterialb, tkcurselection(criterialb))
	}

	setcboptions <- function(man) {
		print(man)
			if(tclvalue(man) == "Cutoff"){
				criteriacboptions <<- sort(c(availableCriteria, "fdrpass","numpass"))

					operatoroptions <<- c("==",">=","<=",">","<","!=")
			}else{
				criteriacboptions <<- sort(availableCriteria)

					operatoroptions <<- c("==",">=","<=",">","<","!=","ASC","DESC")
			}

		repeat{
			if(tclvalue(tkget(criterialb,0)) == ""){
				break
			}else{
				tkdelete(criterialb,0)
			}
		}

		tk2list.set(criteriacb, criteriacboptions)
			tk2list.set(signedcb, criteriacboptions)
			tk2list.set(operatorcb, operatoroptions)
			rmsignedlb()
	}

	fillsignedlb <- function() {
		for (i in (1:length(criteriacboptions)))
		{
			tkinsert(signedlb, "end", criteriacboptions[i])
		}
	}

	rmsignedlb <- function(...) {
		for (j in (1:as.integer(length(criteriacboptions)))){
			tkdelete(signedlb, 0)
		}
		for (i in (1:length(criteriacboptions))){
			if(tclvalue(sign) != criteriacboptions[i]){
				tkinsert(signedlb, "end", criteriacboptions[i])
			}
		}
	}

	setAllPreFilter <- function(num) {

		filter <- paste("PctALL >= ",num)
			PreFilter <<- tclVar(filter)
			tkconfigure(entry.PreFilter, textvariable = PreFilter)
			tkset(onetwodataspin,0)
	}

	setOneTwoPreFilter <- function(num) {

		filter <- paste("PctGrp1 >= ",num,", PctGrp2 >= ",num)
			PreFilter <<- tclVar(filter)
			tkconfigure(entry.PreFilter, textvariable = PreFilter)
			tkset(alldataspin,0)
	}

	buildCriteria <- function() {
		fdrFlag <- FALSE
			numFlag <- FALSE
			repeat{
				if(tclvalue(tkget(criterialb,0)) == ""){
					break
				}else{
					critString <- strsplit(tclvalue(tkget(criterialb,0)), " ")
						nameString <- critString[[1]][1]
						operatorString <- critString[[1]][2]
						if(length(critString[[1]][3]) != 0){
							critValue <- c(critString[[1]][3])
						}else{
							critValue <- NULL
						}
					if(tclvalue(tksize(criterialb)) > 1){
						for(p in (1:(as.integer(tclvalue(tksize(criterialb)))-1))){
							compCritString <- strsplit(tclvalue(tkget(criterialb,p)), " ")
								compNameString <- compCritString[[1]][1]
								compOperatorString <- compCritString[[1]][2]
								if(length(compCritString[[1]][3]) != 0){
									compCritValue <- compCritString[[1]][3]
								}
							if(nameString == compNameString && operatorString == compOperatorString && length(critString[[1]][3]) != 0){
								critValue <- c(critValue, compCritValue)
#tkdelete(criterialb,p)
							}
						}
					}else{				
					}
					newCriteria <- Criteria(name = nameString, operator = operatorString, values = critValue)
						insertFlag <- TRUE

						if(nameString == "fdrpass" && fdrFlag == FALSE){
							fdrPass <<- newCriteria@values
								fdrFlag <- TRUE
						}else if(nameString == "numpass" && numFlag == FALSE ){
							numPass <<- newCriteria@values
								numFlag <- TRUE
						}else{
							if(length(criteria) > 1){
								for(c in (2:(length(criteria)))){
									if(criteria[[c]]@name == nameString && criteria[[c]]@operator == operatorString){
										insertFlag <- FALSE
											break
									}
								}
							}
							if(insertFlag == TRUE){
								criteria <<- c(criteria, newCriteria)
							}
						}
					tkdelete(criterialb,0)
				}
			}
	}

	buildSign <- function() {
		topSign <- tclvalue(tkget(signedcb))
			signs <<- c(signs, topSign)
			for(s in (0:(as.integer(tclvalue(tksize(signedlb)))-1))){
				if(tclvalue(tkselection.includes(signedlb, s)) == 1){
					signs <<- c(signs, tclvalue(tkget(signedlb, s)))
				}
			}
	}

	buildManager <- function(){
		buildCriteria()
			buildSign()

			if(tclvalue(managertype) == "TopN"){
				manager <<- TopNManager(criteria = do.call(c,criteria[-1]), name = tclvalue(name), sign = signs, wfccmfunction = tclvalue(STDFunc), 
						prefilter = tclvalue(PreFilter), permutations = as.numeric(tclvalue(PvalPerm)), topN=as.numeric(tclvalue(TopN)))
					fileName <- paste(manager@name, ".txt")
					fileName <- sub(" .", ".", fileName)
					write.TopNManager(manager, fileName)
			}else{
				manager <<- CutoffManager(criteria = do.call(c,criteria[-1]), name = tclvalue(name), sign = signs, wfccmfunction = tclvalue(STDFunc),
						prefilter = tclvalue(PreFilter), permutations = as.numeric(tclvalue(PvalPerm)), numPass = as.numeric(numPass), fdrPass = as.numeric(fdrPass))
					fileName <- paste(manager@name, ".txt")
					fileName <- sub(" .", ".", fileName)
					write.CutoffManager(manager, fileName)
			}
		tkdestroy(tt)
	}

	getSTDFunc <- function(allCrit, type){
		funcArgs <- c()
			for(i in (1:tclvalue(tksize(criterialb))-1)){
				funcArgs <- c(funcArgs, tclvalue(tkget(criterialb,i)))
			}
		func <- generateSTDFunc(allCrit, funcArgs, type)

			STDFunc <<- tclVar(func)

			tkconfigure(entry.STDFunc, textvariable = STDFunc)
	}

###MAIN
	tt <- tktoplevel()
		criteria <- NULL
		signs <- NULL
		manager <- NULL
		numPass <- NULL
		fdrPass <- NULL
		tkwm.title(tt,"WFCCM")
		managertype <- tclVar("TopN")
#Create Manager Type Dropdown
		tkgrid(tklabel(tt, text="Manager Type"), columnspan=2)
		criteriacboptions <- sort(availableCriteria)
		managercb <- tk2combobox(tt, command= function (...){setcboptions(managertype)})
		tkgrid(managercb, columnspan=2)
		managercboptions <- c("TopN","Cutoff")
		tk2list.set(managercb, managercboptions)
		tkconfigure(managercb, textvariable = managertype)

#Create Box to Accept TopN
		TopN <- tclVar("100")
		entry.TopN <- tkentry(tt,width="10",textvariable=TopN)
		TopNLabel <- tklabel(tt,text="TopN")
		tkgrid(TopNLabel,entry.TopN)
		tkgrid.configure(entry.TopN, sticky ="w")
		tkgrid.configure(TopNLabel, sticky = "e")

#Insert a Blank Row
		tkgrid(tklabel(tt,text=""))

#Add Criteria Dropdown
		tkgrid(tklabel(tt,text="Add Criteria"),columnspan=2)
		criteriacb <- tk2combobox(tt)
		tkgrid(criteriacb,columnspan=2)

		tk2list.set(criteriacb, criteriacboptions)
		criteria <- tclVar("asam")
		tkconfigure(criteriacb, textvariable = criteria)

#Add Operator Dropdown
		operatorcb <- tk2combobox(tt)
		tkgrid(operatorcb,columnspan=2)
		operatoroptions <- c("==",">=","<=",">","<","!=","ASC","DESC")
		tk2list.set(operatorcb, operatoroptions)
		operator <- tclVar("==")
		tkconfigure(operatorcb, textvariable = operator)

#Add Value Box
		value <-tclVar()
		entry.value <- tkentry(tt,width="10",textvariable=value)
		valueLabel <- tklabel(tt,text="Value")
		tkgrid(valueLabel,entry.value)
		tkgrid.configure(valueLabel, sticky="e")
		tkgrid.configure(entry.value, sticky="w")

#Add Add/Remove Buttons
		Add.but <- tk2button(tt,text="Add", width="5", command= function() addcriteria(criteria,operator,value))
		Remove.but <- tk2button(tt,text="Remove", width="5", command= function() removecriteria())
		tkgrid(tklabel(tt,text=""),Add.but)
		tkgrid(tklabel(tt,text=""),Remove.but)
		tkgrid.configure(Add.but, sticky="e")
		tkgrid.configure(Remove.but, sticky="e")

#Insert Blank Line
		tkgrid(tklabel(tt,text=""))

#Add Distance Pvalue Perm. #
		tkgrid(tklabel(tt,text="Distance Pvalue"), columnspan=2)
		tkgrid(tklabel(tt,text="Permutation Number"), columnspan=2)
		PvalPerm <- tclVar(10000)
		PvalPermSpinbox <- tk2spinbox(tt, from = 0, to = Inf, increment =1)
		tkgrid(PvalPermSpinbox, columnspan=2)
		tkconfigure(PvalPermSpinbox, textvariable = PvalPerm)

#Add % Data boxes
		alldataspin <- tk2spinbox(tt, from = 0, to = 100, increment=1, width=5, command= function(){setAllPreFilter(tclvalue(tkget(alldataspin)))})
		tkgrid(tklabel(tt,text="% Data: All"), alldataspin)
		tkgrid.configure(alldataspin, sticky="w")

		onetwodataspin <- tk2spinbox(tt, from = 0, to = 100, increment=1, width=5, command= function(){setOneTwoPreFilter(tclvalue(tkget(onetwodataspin)))})
		tkgrid(tklabel(tt,text="% Data:\n Groups 1 & 2"),onetwodataspin)
		tkgrid.configure(onetwodataspin, sticky="w")

#Add empty column for spacing
		tkgrid(tklabel(tt,text=""),column=2,row=0)

#Add Criteria Listbox
		tkgrid(tklabel(tt,text="Criteria"),column=3,row=0)
		criterialb <- tk2listbox(tt, height=20, selectmode="single")
		tkgrid(criterialb, column=3, row=0,rowspan=16)
		tkgrid.configure(criterialb, sticky="s")



#Add empty column for spacing
		tkgrid(tklabel(tt,text=""),column=4,row=0)

#Add Signed Drop Down and List box
		tkgrid(tklabel(tt,text="Signed"), column=5,row=0)
		sign <- tclVar("")
		prevsign <- ""
		signedcb <- tk2combobox(tt, command = rmsignedlb)
		tkgrid(signedcb, column=5, row =1)
		signedoptions <- criteriacboptions
		tk2list.set(signedcb, signedoptions)
		tkconfigure(signedcb, textvariable = sign)

		scr <- tkscrollbar(tt, repeatinterval=5,command=function(...)tkyview(signedlb,...))
		signedlb <- tklistbox(tt, height=18, selectmode="multiple",exportselection=0,yscrollcommand=function(...)tkset(scr,...))
		fillsignedlb()
		tkgrid(signedlb, column=5,row=2, rowspan=14)
		tkgrid(scr, column=6,row=2, rowspan=14)
		tkgrid.configure(signedlb, sticky ="s")
		tkgrid.configure(scr, sticky="e")

#Add Prefilter Text Entry
		tkgrid(tklabel(tt,text="PreFilter"),columnspan=2)
		PreFilter <-tclVar()
		entry.PreFilter <- tkentry(tt,width="75",textvariable=PreFilter)
		tkgrid(entry.PreFilter, columnspan=6)

#Add STD Function text entry
		tkgrid(tklabel(tt,text="STD Function"),columnspan=2)
		STDFunc <-tclVar()
		entry.STDFunc <- tkentry(tt,width="75",textvariable=STDFunc)
		tkgrid(entry.STDFunc, columnspan=6)

		tkgrid(tklabel(tt,text=""))

#Add "Generate STD Function" Button
		stdfuncbut <- tk2button(tt, text="Generate STD Function", width=20, command= function(){getSTDFunc(criteriacboptions, managertype)})
		tkgrid(stdfuncbut, column=3)

		tkgrid(tklabel(tt,text=""))



#Add Name text entry
		tkgrid(tklabel(tt,text="Criteria Set Name"),columnspan=2)
		name <-tclVar()
		entry.name <- tkentry(tt,width="30",textvariable=name)
		tkgrid(entry.name, columnspan=3)

#Add OK Button
		okbut <- tk2button(tt, text="Save", width=10, command = buildManager)
		tkgrid(okbut, column=3)

		tkfocus(tt)
	}

	main()

})
}
