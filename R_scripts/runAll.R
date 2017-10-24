# Script to run all simulations and analyses and generate figures for Buelo et al. 2017 L&O Letters manuscript
# Cal Buelo, cbuelo10@gmail.com, 10/17/17

startTime = Sys.time()
setwd("path/to/SpatialBloomIndicators")
# install.packages(c("foreach", "doParallel", "fields", "plyr", "moments", "gstat", "sp", "ggplot2", "cowplot", "scales"))
#source needed scripts
source(file.path(".", "R_scripts", "simulation", "helperFunctions.R"))
source(file.path(".", "R_scripts", "simulation", "makeEddyField.R"))
source(file.path(".", "R_scripts", "simulation", "calc_partialDerivs.R"))
source(file.path(".", "R_scripts", "simulation", "evaluateDerivative.R"))
source(file.path(".", "R_scripts", "simulation", "euler.R"))
source(file.path(".", "R_scripts", "simulation", "initializeSystem.R"))
# ========================================
# Set variables
# ========================================
# script takes ~ 15 hours (3 hours for simulations + 12 hours for statistics) to run total on a machine with 32 cores
# time can be decreased by using pre-run starting points, stochastic simulations, and statistical calculations
# intermediate files (deterministic and stochastic model runs, calculated indicators) can be downloaded from Figshare at <url here>
makeEddies = TRUE
calcDeterministicStartingPoints = TRUE # setting to FALSE saves 2 hours
runStochasticSimulations = TRUE # setting to FALSE saves 1 hours
calcIndicators = TRUE # setting to FALSE saves 12 hours
calc_ACrange_initialValueGuess = TRUE # if calcIndicators is TRUE but calc_ACrange_initialValueGuess is FALSE, uses pre-calculated guesses for ACrange; saves ~ 6 hours
makeFigures = TRUE

# set parameters for input rate and step size for euler integration
inputRates = seq(from=0.3, to=1.8, by=0.05)
stepSize = 0.025

# set up grid
sideLength = 200
numberOfGridCells = 180 # 90 on each side axes (ignoring 1 on the axes)
xmax = sideLength / 2
xmin = -sideLength / 2
ymax = sideLength / 2
ymin = -sideLength / 2
dx = sideLength / (numberOfGridCells + 1)
dy = sideLength / (numberOfGridCells + 1)
gridX = seq(from=(xmin + dx/2), to=(xmax-dx/2), by=dx)
gridY = seq(from=(ymin + dy/2), to=(ymax-dy/2), by=dy)

grid_x = matrix(data=gridX, nrow=numberOfGridCells+1, ncol=numberOfGridCells+1, byrow=TRUE)
grid_y = matrix(data=rev(gridY), nrow=numberOfGridCells+1, ncol=numberOfGridCells+1, byrow=FALSE)

# all other variables
NumberOfEddies = 100
NumberOfSteps_deterministic = 40000 # NumberOfSteps * StepSize[line 16] = 1000 time units
NumberOfSteps_stochastic = 20000

deterministicRuns_SaveDirectory_baseCase = "DeterministicRuns_baseCase"
deterministicRuns_SaveDirectory_highDiffusion = "DeterministicRuns_highDiffusion" 
deterministicRuns_SaveDirectory_lowAdvection = "DeterministicRuns_lowAdvection"

stochasticRuns_SaveDirectory_baseCase = "StochasticRuns_baseCase"
stochasticRuns_SaveDirectory_highDiffusion = "StochasticRuns_highDiffusion"
stochasticRuns_SaveDirectory_lowAdvection = "StochasticRuns_lowAdvection"

ACrange_baseCase_guessNA = "stoch_base_ACr_guessNA.csv"
ACrange_highDiff_guessNA = "stoch_highD_ACr_guessNA.csv"
ACrange_lowAdv_guessNA = "stoch_lowA_ACr_guessNA.csv"

ACrange_bestGuess_baseCaseFile = "baseCase_ACr_bestGuess.csv"
ACrange_bestGuess_highDiffFile = "highDiff_ACr_bestGuess.csv"
ACrange_bestGuess_lowAdvFile = "lowAdv_ACr_bestGuess.csv"

baseCase_spatialIndicators_FinalFileName = "baseCase_spatialIndicators.csv"
highDiff_spatialIndicators_FinalFileName = "highDiffusion_spatialIndicators.csv"
lowAdv_spatialIndicators_FinalFileName = "lowAdvection_spatialIndicators.csv"

fig1_time = 100
fig1_lowInputFile = paste("input0.35_stepSize0.025_", NumberOfSteps_stochastic, "steps_writeTime1_multiplicativeNoise_sigma0.01_equilibriumStart.RData", sep="")
fig1_mediumInputFile = paste("input0.9_stepSize0.025_", NumberOfSteps_stochastic, "steps_writeTime1_multiplicativeNoise_sigma0.01_equilibriumStart.RData", sep="")
fig1_highInputFile = paste("input1.6_stepSize0.025_", NumberOfSteps_stochastic, "steps_writeTime1_multiplicativeNoise_sigma0.01_equilibriumStart.RData", sep="")

spatialIndicators_burnInTime = 100
fig2_maxTime=300
# ========================================
# Set up parallel computation
# ========================================
# for an X core computer will run on X - 1 cores
library(foreach)
library(doParallel)

cores=detectCores()
c1 = makeCluster(cores[1]-1)
registerDoParallel(c1)

# ========================================
# Create eddies
# ========================================
if(makeEddies == TRUE){

	source(file.path(".", "R_scripts", "simulation", "makeEddyField.R"))
	# base eddy
	eddy = makeEddy()
	save(eddy, file=file.path(".", "Inputs", "eddy_baseCase.RData"))

	#low advection eddy
	eddy = 0.5*makeEddy()
	save(eddy, file=file.path(".", "Inputs", "eddy_lowAdvection.RData"))
}
# ========================================
# Deterministic runs for starting points
# ========================================
if(calcDeterministicStartingPoints == TRUE){

	source(file.path(".", "R_scripts", "simulation", "runSimulation.R"))
	source(file.path(".", "R_scripts", "simulation", "aggregate_startingPoints.R"))
	
	# run deterministic simulations for base case
	foreach(run = 1:length(inputRates)) %dopar%{
	  runSim(inputRate=inputRates[run], numSteps=NumberOfSteps_deterministic, stepSize=stepSize, writeOutput=TRUE, writeDir=deterministicRuns_SaveDirectory_baseCase, writeEvery_timeUnits=10, initializeMethod="serizawa", eddyMethod="file", eddyFile="./Inputs/eddy_baseCase.RData", stochastic=FALSE)
	}
	aggregateTimePoints(directoryToAggregate=deterministicRuns_SaveDirectory_baseCase, writeFileName = "deterministicStartingPoints_baseCase", timeToPull = 1000)

	# run deterministic simulations for high diffusion case
	foreach(run = 1:length(inputRates)) %dopar%{
	  runSim(inputRate=inputRates[run], numSteps=NumberOfSteps_deterministic, stepSize=stepSize, writeOutput=TRUE, writeDir=deterministicRuns_SaveDirectory_highDiffusion, writeEvery_timeUnits=10, initializeMethod="serizawa", eddyMethod="file", eddyFile="./Inputs/eddy_baseCase.RData", stochastic=FALSE, DN=0.08, DP=0.08)
	}
	aggregateTimePoints(directoryToAggregate=deterministicRuns_SaveDirectory_highDiffusion, writeFileName = "deterministicStartingPoints_highDiffusion", timeToPull = 1000)

	# run deterministic simulations for low advection case
	foreach(run = 1:length(inputRates)) %dopar%{
	  runSim(inputRate=inputRates[run], numSteps=NumberOfSteps_deterministic, stepSize=stepSize, writeOutput=TRUE, writeDir=deterministicRuns_SaveDirectory_lowAdvection, writeEvery_timeUnits=10, initializeMethod="serizawa", eddyMethod="file", eddyFile="./Inputs/eddy_lowAdvection.RData", stochastic=FALSE)
	}
	aggregateTimePoints(directoryToAggregate=deterministicRuns_SaveDirectory_lowAdvection, writeFileName = "deterministicStartingPoints_lowAdvection", timeToPull = 1000)
}

# ========================================
# Stochastic runs
# ========================================
if(runStochasticSimulations == TRUE){
	
	# base case runs
	load(file.path(".", "Inputs", "deterministicStartingPoints_baseCase_t1000.RData"))
	foreach(run = 1:length(inputRates)) %dopar%{
	  runSim(inputRate=inputRates[run], numSteps=NumberOfSteps_stochastic, stepSize=stepSize, writeOutput=TRUE, writeDir=stochasticRuns_SaveDirectory_baseCase, initializeMethod="equilibrium", initialConditions = init_conds[,,,as.character(inputRates[run])], eddyMethod="file", eddyFile="./Inputs/eddy_baseCase.RData", stochastic=TRUE, sigma=0.01)
	}

	# high diffusion runs
	load(file.path(".", "Inputs", "deterministicStartingPoints_highDiffusion_t1000.RData"))
	foreach(run = 1:length(inputRates)) %dopar%{
	runSim(inputRate=inputRates[run], numSteps=NumberOfSteps_stochastic, stepSize=stepSize, writeOutput=TRUE, writeDir=stochasticRuns_SaveDirectory_highDiffusion, initializeMethod="equilibrium", initialConditions = init_conds[,,,as.character(inputRates[run])], eddyMethod="file", eddyFile="./Inputs/eddy_baseCase.RData", stochastic=TRUE, sigma=0.01, DN=0.08, DP=0.08)
	}

	# low advection runs
	load(file.path(".", "Inputs", "deterministicStartingPoints_lowAdvection_t1000.RData"))
	foreach(run = 1:length(inputRates)) %dopar%{
	  runSim(inputRate=inputRates[run], numSteps=NumberOfSteps_stochastic, stepSize=stepSize, writeOutput=TRUE, writeDir=stochasticRuns_SaveDirectory_lowAdvection, initializeMethod="equilibrium", initialConditions = init_conds[,,,as.character(inputRates[run])], eddyMethod="file", eddyFile="./Inputs/eddy_lowAdvection.RData", stochastic=TRUE, sigma=0.01)
	}
}
# ========================================
# Calculate indicators
# ========================================
if(calcIndicators == TRUE){
	source(file.path(".", "R_scripts", "analysis", "spatialIndicators.R"))
	source(file.path(".", "R_scripts", "analysis", "variogram_RangeGuess.R"))

	if(calc_ACrange_initialValueGuess == TRUE){
		#calculate AC range with no specifiied initial value guess
		calcSpatialIndicators(directoryName=stochasticRuns_SaveDirectory_baseCase, saveFileName=ACrange_baseCase_guessNA, Moments=FALSE, ACrange=TRUE, moransI=FALSE, ACrange_guessFile=NA)
		calcSpatialIndicators(directoryName=stochasticRuns_SaveDirectory_highDiffusion, saveFileName=ACrange_highDiff_guessNA, Moments=FALSE,  ACrange=TRUE, moransI=FALSE, ACrange_guessFile=NA)
		calcSpatialIndicators(directoryName=stochasticRuns_SaveDirectory_lowAdvection, saveFileName=ACrange_lowAdv_guessNA, Moments=FALSE, ACrange=TRUE, moransI=FALSE, ACrange_guessFile=NA)

		#determine best guess
 		variogram_rangeGuess(statsInputFile=ACrange_baseCase_guessNA, guessOutputFile=ACrange_bestGuess_baseCaseFile, roundToNearest=2)
		variogram_rangeGuess(statsInputFile=ACrange_highDiff_guessNA, guessOutputFile=ACrange_bestGuess_highDiffFile, roundToNearest=2)
		variogram_rangeGuess(statsInputFile=ACrange_lowAdv_guessNA, guessOutputFile=ACrange_bestGuess_lowAdvFile, roundToNearest=2)
	}

	# calculate spatial indicators, using guess as starting point
	# base case
	calcSpatialIndicators(directoryName=stochasticRuns_SaveDirectory_baseCase, saveFileName=baseCase_spatialIndicators_FinalFileName, Moments=TRUE, ACrange=TRUE, moransI = TRUE, ACrange_guessFile=ACrange_bestGuess_baseCaseFile)
	calcSpatialIndicators(directoryName=stochasticRuns_SaveDirectory_highDiffusion, saveFileName=highDiff_spatialIndicators_FinalFileName, Moments=TRUE, ACrange=TRUE, moransI = TRUE,  ACrange_guessFile=ACrange_bestGuess_highDiffFile)
	calcSpatialIndicators(directoryName=stochasticRuns_SaveDirectory_lowAdvection, saveFileName=lowAdv_spatialIndicators_FinalFileName, Moments=TRUE, ACrange=TRUE, moransI = TRUE,  ACrange_guessFile=ACrange_bestGuess_lowAdvFile)

}

on.exit(stopCluster(c1))
# ========================================
# Make plots
# ========================================
if(makeFigures == TRUE){
	library(fields)
	library(ggplot2)
	library(cowplot)
	library(scales)

	source(file.path(".", "R_scripts", "analysis", "makePlots.R"))
	makePlots(lowInputFile=fig1_lowInputFile, mediumInputFile=fig1_mediumInputFile, highInputFile=fig1_highInputFile, fig1time=fig1_time, baseCaseFile=baseCase_spatialIndicators_FinalFileName, highDiffFile=highDiff_spatialIndicators_FinalFileName, lowAdvFile=lowAdv_spatialIndicators_FinalFileName, burnInTime=spatialIndicators_burnInTime, fig2maxTime=fig2_maxTime)
}

# ========================================
# End parallel computation
# ========================================
endTime = Sys.time()
totalTime = endTime - startTime
cat(totalTime, file="timeToRun.txt")