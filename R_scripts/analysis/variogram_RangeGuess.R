# function to calculate the (rounded) mode of autocorrelation ranges from calculations done without a initial guess; for use in another round of calculations as the starting point
variogram_rangeGuess <- function(statsInputFile, guessOutputFile, roundToNearest=2){
	inputFilePath = file.path(".", "Outputs", "analyses", statsInputFile)
	range_NAinit = read.csv(inputFilePath, stringsAsFactors=FALSE)

	# round to nearest X
	range_NAinit$ACrange_round = round(range_NAinit$ACrange / roundToNearest) * roundToNearest

	Mode <- function(x) {
	  ux <- unique(x)
	  ux[which.max(tabulate(match(x, ux)))]
	}

	bestGuess = aggregate(ACrange_round~inputRate, data=range_NAinit, FUN=Mode)
	bestGuess$File = statsInputFile

	writeFilePath = file.path(".", "Inputs", guessOutputFile)
	write.csv(bestGuess, writeFilePath, row.names=FALSE)
}