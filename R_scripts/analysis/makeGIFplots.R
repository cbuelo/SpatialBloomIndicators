# script to make plots for an animated GIF of the algal bloom

## Below saves just the phytos to make an animated GIF; run "MakeGIFplots.R" and then on command line: convert *.png -delay 3 -loop 1 ../baseCase_i0.9_t101_to_t190.gif
library(fields)
setwd("/home/cal/Documents/Research/SpatialBloomIndicators/")

File = file.path(".", "Outputs", "simulations", "StochasticRuns_baseCase", "input0.9_stepSize0.025_20000steps_writeTime1_multiplicativeNoise_sigma0.01_equilibriumStart.RData")
load(File)

hold = system_output[ , , , "phytos"]

saveDir = file.path(".", "Figures", "GIFplots")

for(i in 189:194){
	fileName = paste(saveDir, "/phytos", i, ".png", sep="")
	png(fileName)
	image.plot(hold[,,i], zlim=range(hold), main="Base Case, i = 0.9")
	dev.off()
}