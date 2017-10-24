# function to analyze simulation (i.e. grid/grids) for spatial indicators
# CDB, 6/12/17
calcSpatialIndicators <- function(directoryName, saveFileName, ACrange_guessFile=NA, Moments = TRUE, ACrange = TRUE, moransI = TRUE){
	# get files
	filesDirectory = file.path(".", "Outputs", "simulations", directoryName)
	fileNames = list.files(filesDirectory)
	#get best guess for AC range
	if(!is.na(ACrange_guessFile)){
		ACrange_guessFilePath = file.path(".", "Inputs", ACrange_guessFile)
		ACrange_guess = read.csv(ACrange_guessFilePath, stringsAsFactors=FALSE)
	}

	# set up data frame to hold results: columns that go into all results
	colNames = c("inputRate", "time", "directory", "file", "ACrange_guessFile", "stochastic", "sigma")

	# load needed functions depending on which stats running and add in columns for stats to calculate
	libsToLoad = c()
	if(Moments == TRUE){
		library("moments")
	  	libsToLoad = c(libsToLoad, "moments")
		colNames = c(colNames, "mean", "median", "min", "max", "sd", "skew", "kurt")
	}
	if(ACrange == TRUE){
		colNames = c(colNames, "ACrange")
		library(plyr)
		library(sp)
		library(gstat)
		libsToLoad = c(libsToLoad, "plyr", "sp", "gstat")
	}
	if(moransI == TRUE){
		#define Moran's I function
		moranI <- function(input){
		  m <- mean(as.vector(input))
		  v <- var(as.vector(input))
		  n <- (nrow(input)-1)
		  moranI <- 0
		  for (i in 2:n){
		    for (j in 2:n) {
		      moranI <- moranI + (input[i,j]-m)*(input[i,j-1]+input[i,j+1]+input[i-1,j]+input[i+1,j]-4*m)	
		    }
		  }
		  moranI <- moranI/(4*v*(n-2)*(n-2))
		  return(moranI)
		}
	}

	all_results = foreach(f = 1:length(fileNames), .combine=rbind, .packages = libsToLoad, .export = c("numberOfGridCells", "grid_x", "grid_y")) %dopar% {
		load(file.path(filesDirectory, fileNames[f]))
		writeIndices = 1:dim(system_output)[3]

		
		if(ACrange == TRUE){
			#set up system for calculating variogram		  
			System = array(0, c(numberOfGridCells+1, numberOfGridCells+1, 3), dimnames=list(NULL, NULL, c("x", "y", "phytos")))
			System[ , , "x"] = grid_x
			System[ , , "y"] = grid_y
			#if there's a file for best guess for AC range, load it
			if(!is.na(ACrange_guessFile)){
		 		AC_range_guessValue = ACrange_guess[which.min(abs(ACrange_guess$inputRate - paramList$inputRate)), "ACrange_round"]
		 	}else{
				AC_range_guessValue = NA
		  	}
		}

		data_hold = data.frame(matrix(NA, length(writeIndices), length(colNames)))
		colnames(data_hold) = colNames

		for(s in 1:length(writeIndices)){
			index_system = writeIndices[s]
			data_hold[s, "inputRate"] = paramList$inputRate
			data_hold[s, "file"] = fileNames[f]
			data_hold[s, "directory"] = directoryName
			data_hold[s, "stochastic"] = paramList$stochastic
			data_hold[s, "sigma"] = paramList$sigma
			data_hold[s, "time"] = write_times[s]
			data_hold[s, "ACrange_guessFile"] = ACrange_guessFile
			if(all(is.na(system_output[ , , s, "phytos"]))){
			  data_hold[, colNames[!(colNames %in% c("inputRate", "time", "directory", "file", "ACrange_guessFile", "stochastic", "sigma"))]] = NA
			}else{
		  		if(Moments == TRUE){
		  			data_hold[s, "mean"] = mean(system_output[ , , index_system, "phytos"])
		  			data_hold[s, "median"] = median(system_output[ , , index_system, "phytos"])
		  			data_hold[s, "min"] = min(system_output[ , , index_system, "phytos"])
		  			data_hold[s, "max"] = max(system_output[ , , index_system, "phytos"])
		  			data_hold[s, "sd"] = sd(system_output[ , , index_system, "phytos"])
		  			data_hold[s, "skew"] = skewness(as.vector(system_output[ , , index_system, "phytos"]))
		  			data_hold[s, "kurt"] = kurtosis(as.vector(system_output[ , , index_system, "phytos"]))
		  		}
		  		if(ACrange == TRUE){
		  			#calculate AC distance
		  			System[ , , "phytos"] = system_output[ , , index_system, "phytos"]
		  			grid_df = adply(System, 1)[,c("x", "y", "phytos")]
		  			coordinates(grid_df) = ~x+y
		  			v = variogram(phytos~1, grid_df)
		  			v.fit = fit.variogram(v, vgm(NA, c("Exp"), AC_range_guessValue, NA))
		  
		  			data_hold[s, "ACrange"] = v.fit$range[v.fit$model == "Exp"]
		  		}
		  		if(moransI == TRUE){
		  			data_hold[s, "moransI"] = moranI(system_output[ , , index_system, "phytos"])
		  		}
			}
		}
		data_hold
	}
	outputFilePath = file.path(".", "Outputs", "analyses", saveFileName)
	write.csv(all_results, outputFilePath, row.names=FALSE)
}