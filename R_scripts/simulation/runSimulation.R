# function to run simulation
runSim <- function(inputRate, numSteps, stepSize, initializeMethod="serizawa", sideLength=200, initialConditions=NULL, writeOutput=TRUE, writeDir="tempHold", writeEvery_timeUnits=1, eddyMethod="file", eddyFile="./Inputs/eddy_seedEquals2017.RData", returnWrites=FALSE, stochastic=FALSE, sigma=0, modelExplodedThreshold = 100, DN=0.04, DP=0.04, a=8, m=0.03, f=0.9){
	
	#initialize the system
	if(initializeMethod == "serizawa"){
		fixedPoint = getFixedPoint(inputRate=inputRate, A=a, M=m, F=f)
		nutrients_start = initializeSerizawa_nutrients(x=grid_x, y=grid_y, numberOfGridCells=numberOfGridCells, fixedPoint = fixedPoint)
		phytos_start = initializeSerizawa_phytos(x=grid_x, y=grid_y, numberOfGridCells=numberOfGridCells, fixedPoint = fixedPoint)
	}else if(initializeMethod == "equilibrium"){
		nutrients_start = initialConditions[ , , "nutrients"]
		phytos_start = initialConditions[ , , "phytos"]
	}else{
		print("Invalid 'initializeMethod' option: choose 'serizawa' or 'equilibrium'")
	}

	#create the eddy field
	if(eddyMethod == "file"){
		load(eddyFile)
		Eddy = eddy
	}else if(eddyMethod=="makeNew"){
		Eddy = makeEddy(eddyN=NumberOfEddies, eddySeed=2017)
	}else{
		print("Invalid 'eddyMethod' option: choose 'file' or 'makeNew'")
	}
	maxVelocity = max(Eddy[,,"vel_magnitude"])

	#calculate intial partial derivatives
	system_start = calc_partialDerivs(nutrients = nutrients_start, phytos = phytos_start, Dx=dx, Dy=dy)

	# set up data to be output
	writeEvery_X_steps = as.integer(trunc(writeEvery_timeUnits / stepSize))
	numberOfWrites = trunc(numSteps / writeEvery_X_steps) + 1
	system_output = array(NA, c(numberOfGridCells+1, numberOfGridCells+1, numberOfWrites, 2), dimnames=list(NULL, NULL, NULL, c("nutrients", "phytos")))
	write_times = numeric(numberOfWrites)
	write_counter = 1

	sys_hold = system_start
	system_output[ , , write_counter,c("nutrients", "phytos")] = sys_hold[ , , c("nutrients", "phytos"), "stateVar"]
	write_times[write_counter] = 0
	write_counter = write_counter + 1

	#iterate forward in time
	for(steps in 1:numSteps){
		nextState = euler(sysState_start = sys_hold, velField = Eddy, dT=stepSize, I=inputRate, A=a, M=m, Dn=DN, Dp=DP, F=f, gridN=numberOfGridCells, Dx=dx, Dy=dy, stochastic=stochastic, sigma=sigma)
		sys_hold = nextState
		if(steps %% writeEvery_X_steps == 0){
			system_output[ , , write_counter,c("nutrients", "phytos")] = sys_hold[ , , c("nutrients", "phytos"), "stateVar"]
			write_times[write_counter] = steps*stepSize
			write_counter = write_counter + 1
		}
		if(any(abs(sys_hold[ , , "phytos", "stateVar"]) > modelExplodedThreshold)){
		  break
		}
	}
	if(writeOutput == TRUE){
		paramList = list(eddyMethod = eddyMethod, eddyFile=eddyFile, numSteps = numSteps, stepSize = stepSize, inputRate = inputRate, numberOfGridCells = numberOfGridCells, sideLength = sideLength, xmax = xmax, xmin = xmin, ymax = ymax, ymin = ymin, dx = dx, dy = dy, NumberOfEddies = NumberOfEddies, DN = DN, DP = DP, maxVelocity = maxVelocity, a=a, m=m, f=f, stochastic=stochastic, sigma=sigma, initializeMethod=initializeMethod)
		if(stochastic == TRUE){
			noisePart = paste("_multiplicativeNoise", "_sigma", sigma, sep="")
		}else{
			noisePart = "_deterministic"
		}
		fileName = paste("input", inputRate, "_stepSize", stepSize, "_", numSteps, "steps_writeTime", writeEvery_timeUnits, noisePart, "_", initializeMethod, "Start", ".RData", sep="")
		dirPath = file.path(".", "Outputs", "simulations", writeDir)
		dir.create(dirPath)
		save(system_output, write_times, paramList, file=file.path(dirPath, fileName))
	}
	if(returnWrites == TRUE){
		return(system_output)
	}
}