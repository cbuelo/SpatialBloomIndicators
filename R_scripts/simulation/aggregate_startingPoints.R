# function to aggregate long-run system states from deterministic runs, to be used as starting points for stochastic simulations
aggregateTimePoints <- function(directoryToAggregate, writeFileName, timeToPull){
	directoryPath = file.path(".", "Outputs", "simulations", directoryToAggregate)
	fileNames = list.files(directoryPath)

	#set up array to hold initial conditions
	init_conds = array(NA, c(numberOfGridCells+1, numberOfGridCells+1, 2, length(fileNames)), dimnames=list(NULL, NULL, c("nutrients", "phytos"), NULL))
	input_rates = rep(NA, length(fileNames))

	#loop through files and extract time wanted
	for(f in 1:length(fileNames)){
		load(file.path(directoryPath, fileNames[f]))
		hold_sys = system_output[ , , which.min(abs(write_times - timeToPull)), c("nutrients", "phytos")]
		hold_inputRate = paramList$inputRate

		init_conds[ , , c("nutrients", "phytos"), f] = hold_sys
		input_rates[f] = hold_inputRate
	}
	dimnames(init_conds)[[4]] = input_rates

	#output initial conditions
	formattedFileName = paste(writeFileName, "_t", timeToPull, ".RData", sep="")
	writeFilePath = file.path(".", "Inputs", formattedFileName)
	save(init_conds, input_rates, fileNames, file=writeFilePath)
}