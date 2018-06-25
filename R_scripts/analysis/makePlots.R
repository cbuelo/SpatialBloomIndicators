# function to make plots for manuscript
makePlots <- function(lowInputFile, mediumInputFile, highInputFile, fig1time=100, baseCaseFile, highDiffFile, lowAdvFile, burnInTime=100, fig2maxTime=300){

	# load results
	baseCase0 = read.csv(file.path(".", "Outputs", "analyses", baseCaseFile), stringsAsFactors=FALSE)
	highDiff0 = read.csv(file.path(".", "Outputs", "analyses", highDiffFile), stringsAsFactors=FALSE)
	lowAdv0 = read.csv(file.path(".", "Outputs", "analyses", lowAdvFile), stringsAsFactors=FALSE)

	baseCase = baseCase0[baseCase0$time > burnInTime, ]
	highDiff = highDiff0[highDiff0$time > burnInTime,]
	lowAdv = lowAdv0[lowAdv0$time > burnInTime,]


	# ===============================================
	# Figure 1: example grids
	# ===============================================
	load(file.path(".", "Outputs", "simulations", "StochasticRuns_baseCase", lowInputFile))
	grid_low = system_output[, , fig1time, "phytos"]
	load(file.path(".", "Outputs", "simulations", "StochasticRuns_baseCase", mediumInputFile))
	grid_mod = system_output[, , fig1time, "phytos"]
	load(file.path(".", "Outputs", "simulations", "StochasticRuns_baseCase", highInputFile))
	grid_high = system_output[, , fig1time, "phytos"]

	Range = range(c(grid_low, grid_mod, grid_high))
  # plot grids
	jpeg(file.path(".", "Figures", "Figure1.jpg"), width=1250, height=400)
	par(mfrow=c(1,3), oma=c(1,2,1,11), mar=c(1.5,1.5,1.5,1.5))
	image(grid_low, zlim=Range, col=tim.colors(), axes=FALSE) 
	text(diff(par("usr")[1:2])*.1, sum(par("usr")[3:4])*.85, labels="A", cex=5, col="white") 
	image(grid_mod, zlim=Range, col=tim.colors(), axes=FALSE)
	text(diff(par("usr")[1:2])*.1, sum(par("usr")[3:4])*.85, labels="B", cex=5, col="white") 
	image(grid_high, zlim=Range, col=tim.colors(), axes=FALSE)
	text(diff(par("usr")[1:2])*.1, sum(par("usr")[3:4])*.85, labels="C", cex=5, col="white")
  # add legend
	par(oma=c(0,0,0,0))
	image.plot( legend.only=TRUE, zlim=Range, smallplot=c(0.77, 0.8, 0.1, 0.9), legend.lab = "Phytoplankton conc.", legend.cex = 1.5, legend.line = 6, axis.args = list(cex.axis=1.3))

	dev.off()


	# ===============================================
	# Figure 2: Squeal stats through time
	# ===============================================
	Range_mean0 = range(baseCase[baseCase$inputRate %in% c(0.35, 0.9, 1.6), "mean"])
	Range_mean = Range_mean0
	Range_sd0 = range(baseCase[baseCase$inputRate %in% c(0.35, 0.9, 1.6), "sd"])
	Range_sd = Range_sd0
	Range_acR0 = range(baseCase[baseCase$inputRate %in% c(0.35, 0.9, 1.6), "ACrange"])
	Range_acR = Range_acR0


	jpeg(file.path(".", "Figures", "Figure2.jpg"), width=800, height=1200)
	par(mfrow=c(3,1), mar=c(5,11,1,1), oma=c(4,1,1,1), mgp=c(4,2.5,0))
	plot(baseCase[baseCase$inputRate == 0.35 & baseCase$time <= fig2maxTime, c("time", "mean")], ylim=Range_mean, col="#534ED9", type="l", lwd=12, xlab="", ylab="", cex.lab=4, cex.axis=3) #deepskyblue3, slateblue2
	mtext("Mean", side=2, outer=FALSE, cex=3, line=7)
	lines(baseCase[baseCase$inputRate == 0.9 & baseCase$time <= fig2maxTime, c("time", "mean")], ylim=Range_mean, col="#FF9500", lwd=12) #tomato2, goldenrod2, orange, darkorange2
	lines(baseCase[baseCase$inputRate == 1.6 & baseCase$time <= fig2maxTime, c("time", "mean")], ylim=Range_mean, col="#00AC6B", lwd=12) #orchid2, violetred, mediumpurple2
	text(diff(par("usr")[1:2])*.025 + par("usr")[1], diff(par("usr")[3:4])*.875 + par("usr")[3], labels="A", cex=5, col="black")
	legend(x=fig2maxTime-50, y=0.25, legend = c("i = 0.35", "i = 0.9", "i = 1.6"), col=c("#534ED9", "#FF9500", "#00AC6B"), lwd=8, bty="n", cex=2.5)

	plot(baseCase[baseCase$inputRate == 0.35 & baseCase$time <= fig2maxTime, c("time", "sd")], ylim=Range_sd, col="#534ED9", type="l", lwd=12, xlab="", ylab="", cex.lab=4, cex.axis=3)
	mtext("Standard  Deviation", side=2, outer=FALSE, cex=3, line=7)
	lines(baseCase[baseCase$inputRate == 0.9 & baseCase$time <= fig2maxTime, c("time", "sd")], ylim=Range_sd, col="#FF9500", lwd=12)
	lines(baseCase[baseCase$inputRate == 1.6 & baseCase$time <= fig2maxTime, c("time", "sd")], ylim=Range_sd, col="#00AC6B", lwd=12)
	text(diff(par("usr")[1:2])*.025 + par("usr")[1], diff(par("usr")[3:4])*.925 + par("usr")[3], labels="B", cex=5, col="black") 

	plot(baseCase[baseCase$inputRate == 0.35 & baseCase$time <= fig2maxTime, c("time", "ACrange")], ylim=Range_acR, col="#534ED9", type="l", lwd=12, xlab="", ylab="", cex.lab=4, cex.axis=3)
	mtext("AC  Range", side=2, outer=FALSE, cex=3, line=7)
	lines(baseCase[baseCase$inputRate == 0.9 & baseCase$time <= fig2maxTime, c("time", "ACrange")], ylim=Range_acR, col="#FF9500", lwd=12)
	lines(baseCase[baseCase$inputRate == 1.6 & baseCase$time <= fig2maxTime, c("time", "ACrange")], ylim=Range_acR, col="#00AC6B", lwd=12)#, lty="dashed")
	text(diff(par("usr")[1:2])*.025 + par("usr")[1], diff(par("usr")[3:4])*.925 + par("usr")[3], labels="C", cex=5, col="black") 

	mtext("Time", side=1, outer=TRUE, cex=3, line=1.75)
	dev.off()

	# ====================================================================
	# Figure 3 : Physical forcing comparison
	# ====================================================================
	lowAdv$Case = "Lower Advection"
	highDiff$Case = "Higher Diffusion"
	baseCase$Case = "Base Case"

	combined_phys0 = rbind(baseCase, lowAdv)
	combined_phys = rbind(combined_phys0, highDiff)

	# combined_phys = combined_phys1[combined_phys1$time > 0, ]

	#subset number of inputs b/c too many in boxplot
	i_keep = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8)
	combined_phys = combined_phys[combined_phys$inputRate %in% i_keep,]

	combined_phys$inputRate = as.factor(combined_phys$inputRate)
	 
	cbPalette = c("#00AC6B", "#FF9500", "#534ED9")

	p1_phys = ggplot(combined_phys, aes(x=inputRate, y=mean, color=Case)) + 
		theme_classic() + 
		geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6)) + 
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=16)) + 
		# theme(plot.margin = unit(c(0.2, 0.2, 0.45, 0.45), "in")) + 
		theme(axis.title.y=element_text(hjust=0.5, vjust=0.5, size=26, margin=margin(r=15))) + 
		geom_vline(xintercept=c(3, 10.5), col="grey", linetype="dashed") + 
		theme(legend.position="none") + 
		theme(axis.title.x=element_blank()) + 
		theme(plot.margin=unit(c(0.65, 0.5, 1.5, 0.25), "lines")) + 
		scale_colour_manual(values=cbPalette) + 
		ylab("Mean") + 
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5, margin=margin(r=5))) +
		annotate("text",x=1,y=1*max(combined_phys$mean),hjust=.2,label="A", size=12)

	p2_phys = ggplot(combined_phys, aes(x=inputRate, y=sd, color=Case)) + 
		theme_classic() + 
		geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6)) + 
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=16)) + 
		theme(plot.margin = unit(c(0.2, 0.2, 0.45, 0.45), "in")) + 
		theme(axis.title.y=element_text(hjust=0.5, vjust=0.5, size=26, margin=margin(r=15))) + 
		geom_vline(xintercept=c(3, 10.5), col="grey", linetype="dashed") + theme(legend.position="none") + 
		theme(axis.title.x=element_blank()) + 
		theme(plot.margin=unit(c(0.65, 0.5, 1.5, 0.25), "lines")) + 
		theme(legend.position=c(0.8, 0.7), legend.key.size=unit(3, "lines"), legend.text=element_text(size=14), legend.title=element_text(size=18), legend.key.width=unit(0.75, "lines")) + 
		scale_colour_manual(values=cbPalette) +
		ylab("Standard Deviation") +
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5, margin=margin(r=5))) +
		annotate("text",x=1,y=1*max(combined_phys$sd),hjust=.2,label="B", size=12)


	p3_phys = ggplot(combined_phys, aes(x=inputRate, y=ACrange, color=Case)) + 
		theme_classic() + 
		geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6)) + 
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=16)) + 
		theme(axis.title.y=element_text(hjust=0.5, vjust=0.5, size=26, margin=margin(r=15))) + 
		geom_vline(xintercept=c(3, 10.5), col="grey", linetype="dashed") + 
		theme(legend.position="none") + 
		theme(axis.title.x=element_blank()) + 
		theme(plot.margin=unit(c(0.65, 0.5, 1.5, 0.25), "lines")) + 
		scale_colour_manual(values=cbPalette)+
	  scale_y_log10()+
		ylab("AC Range") +
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5, margin=margin(r=5))) +
		annotate("text", x=1, y=1*max(combined_phys$ACrange,na.rm=TRUE), hjust=.2, label="C", size=12)


	p4_phys = ggplot(combined_phys, aes(x=inputRate, y=moransI, color=Case)) + 
		theme_classic() + geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6)) +  
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=16)) + 
		# theme(plot.margin = unit(c(0.2, 0.2, 0.45, 0.45), "in")) + 
		theme(axis.title.y=element_text(hjust=0.5, vjust=0.5, size=26, margin=margin(r=15))) + 
		geom_vline(xintercept=c(3, 10.5), col="grey", linetype="dashed") + 
		theme(legend.position="none") + 
		theme(axis.title.x=element_blank()) + 
		theme(plot.margin=unit(c(0.65, 0.5, 1.5, 0.25), "lines")) + 
		scale_colour_manual(values=cbPalette)+
		ylab("Moran's I") +
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5, margin=margin(r=5))) +
		annotate("text",x=1,y=1*max(combined_phys$moransI),hjust=.2,label="D", size=12)

	p5_phys = ggplot(combined_phys, aes(x=inputRate, y=skew, color=Case)) + 
		theme_classic() + 
		geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6)) +  
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=16)) + 
		# theme(plot.margin = unit(c(0.2, 0.2, 0.45, 0.45), "in")) + 
		theme(axis.title.y=element_text(hjust=0.5, vjust=0.5, size=26, margin=margin(r=15))) + 
		geom_vline(xintercept=c(3, 10.5), col="grey", linetype="dashed") + 
		theme(legend.position="none") + 
		theme(axis.title.x=element_blank()) + 
		theme(plot.margin=unit(c(0.65, 0.5, 1.5, 0.25), "lines")) + 
		scale_colour_manual(values=cbPalette) +
		ylab("Skewness") +
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5, margin=margin(r=5))) +
		annotate("text",x=1,y=1*max(combined_phys$skew),hjust=.2,label="E", size=12)

	p6_phys = ggplot(combined_phys, aes(x=inputRate, y=kurt, color=Case)) + 
		theme_classic() + geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6)) +  
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=16)) + 
		# theme(plot.margin = unit(c(0.2, 0.2, 0.45, 0.45), "in")) + 
		theme(axis.title.y=element_text(hjust=0.5, vjust=0.5, size=26, margin=margin(r=15))) + 
		geom_vline(xintercept=c(3, 10.5), col="grey", linetype="dashed") + 
		theme(legend.position="none") + 
		theme(axis.title.x=element_blank()) + 
		theme(plot.margin=unit(c(0.65, 0.5, 1.5, 0.25), "lines")) + 
		scale_colour_manual(values=cbPalette)+
		ylab("Kurtosis") +
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5, margin=margin(r=5))) +
		annotate("text",x=1,y=1*max(combined_phys$kurt),hjust=.2,label="F", size=12)

	jpeg(file.path(".", "Figures", "Figure3.jpg"), width=1500, height=1000)
	panels = plot_grid(p1_phys, p2_phys, p3_phys, p4_phys, p5_phys, p6_phys, ncol=2, align="vh")
	ggdraw(add_sub(panels, "Phosphorus Input Rate (i)", vpadding=grid::unit(.25,"lines"),y=0.5, x=0.5, vjust=0, size=24))
	dev.off()

	# ====================================================================
	# Figure 4 : Close up of squeal stats that give early warning
	# ====================================================================
	lowI = baseCase[baseCase$inputRate <= 0.7, ]
	highI = baseCase[baseCase$inputRate >= 1.05 & baseCase$inputRate <= 1.45, ]

	lowI$inputRate = as.factor(lowI$inputRate)
	highI$inputRate = as.factor(highI$inputRate)


	lab_seq_liSD = 10^(seq(-4, -1, 1))
	lowI_sd = ggplot(lowI, aes(x=inputRate, y=sd), log="y") + 
		theme_classic() + 
		geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6), color=cbPalette[1]) +  
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=16)) + 
		theme(plot.margin = unit(c(0.2, 0.2, 0.45, 0.45), "in")) + 
		theme(axis.title.y=element_text(hjust=0.5, size=26)) + 
		geom_vline(xintercept=c(5), col="grey", linetype="dashed") + 
		theme(legend.position="none") + 
		theme(axis.title.x=element_blank()) + 
		theme(plot.margin=unit(c(0.25, 0.5, 1.5, 0.25), "lines")) + 
		theme(legend.position=c(0.8, 0.7), legend.key.size=unit(3, "lines"), legend.text=element_text(size=14), legend.title=element_text(size=18), legend.key.width=unit(0.75, "lines")) + 
		scale_colour_manual(values=cbPalette[2]) + 
		scale_y_log10(name="Standard Deviation", limits=c(0.0001, 0.1), breaks=lab_seq_liSD, labels = trans_format("log10", math_format(10^.x))) + 
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
		theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
		annotate("text",x=1,y=1*max(lowI$sd), vjust=-0.2, hjust=.2,label="A", size=12)

	lab_seq_hiSD = 10^(seq(-3, -1, 0.5))
	highI_sd = ggplot(highI, aes(x=inputRate, y=sd), log="y") + 
		theme_classic() + 
		geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6), color=cbPalette[1]) + 
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=10)) + 
		theme(plot.margin = unit(c(0.2, 0.2, 0.45, 0.45), "in")) + 
		theme(axis.title.y=element_text(hjust=0.5, size=26)) + 
		geom_vline(xintercept=c(5), col="grey", linetype="dashed") + theme(legend.position="none") + 
		theme(axis.title.x=element_blank()) + 
		theme(plot.margin=unit(c(0.25, 0.5, 1.5, 0.25), "lines")) + 
		theme(legend.position=c(0.8, 0.7), legend.key.size=unit(3, "lines"), legend.text=element_text(size=14), legend.title=element_text(size=18), legend.key.width=unit(0.75, "lines")) + 
		scale_colour_manual(values=cbPalette[2]) + 
		scale_y_log10(name="Standard Deviation", limits=c(0.003, 0.1), breaks=lab_seq_hiSD, labels = trans_format("log10", math_format(10^.x))) + 
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5), axis.text=element_text(size = 16)) +
		annotate("text",x=1,y=1.0*max(highI$sd), vjust=-0.2, hjust=.2,label="B", size=12)

	lab_seq_liACd = 10^(seq(0, 3, 0.5))
	lowI_ACd = ggplot(lowI, aes(x=inputRate, y=ACrange), log="y") + 
		theme_classic() + 
		geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6), color=cbPalette[1]) + 
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=10)) + 
		theme(plot.margin = unit(c(0.2, 0.2, 0.45, 0.45), "in")) + 
		theme(axis.title.y=element_text(hjust=0.5, size=26)) + 
		geom_vline(xintercept=c(5), col="grey", linetype="dashed") + theme(legend.position="none") + 
		theme(axis.title.x=element_blank()) + 
		theme(plot.margin=unit(c(0.25, 0.5, 1.5, 0.25), "lines")) + 
		theme(legend.position=c(0.8, 0.7), legend.key.size=unit(3, "lines"), legend.text=element_text(size=14), legend.title=element_text(size=18), legend.key.width=unit(0.75, "lines")) + 
		scale_colour_manual(values=cbPalette[2]) + 
		scale_y_log10(name="AC Range", limits=c(1, 50), breaks=lab_seq_liACd, labels = trans_format("log10", math_format(10^.x))) + 
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5), axis.text=element_text(size = 16)) +
		annotate("text",x=1,y=1*max(lowI$ACrange), vjust=-0.2, hjust=.2,label="C", size=12)


	lab_seq_hiACd = 10^(seq(0, 3, 0.5))
	highI_ACd = ggplot(highI, aes(x=inputRate, y=ACrange), log="y") + 
		theme_classic() + geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6), color=cbPalette[1]) +  
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=10)) + 
		theme(plot.margin = unit(c(0.2, 0.2, 0.45, 0.45), "in")) + 
		theme(axis.title.y=element_text(hjust=0.5, size=26)) + 
		geom_vline(xintercept=c(5), col="grey", linetype="dashed") + theme(legend.position="none") + theme(axis.title.x=element_blank()) + theme(plot.margin=unit(c(0.25, 0.5, 1.5, 0.25), "lines")) + 
		theme(legend.position=c(0.8, 0.7), legend.key.size=unit(3, "lines"), legend.text=element_text(size=14), legend.title=element_text(size=18), legend.key.width=unit(0.75, "lines")) + 
		scale_colour_manual(values=cbPalette[2]) + 
		scale_y_log10(name="AC Range", limits=c(1.1, 175), breaks=lab_seq_hiACd, labels = trans_format("log10", math_format(10^.x))) + 
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5), axis.text=element_text(size = 16)) +
		annotate("text",x=1,y=1*max(highI$ACrange), vjust=-0.2, hjust=.2,label="D", size=12)

	# just check what Moran's I looks like
	lab_seq_lowMoransI = 10^(seq(-1, 0, 0.25))
	lowI_moransI = ggplot(lowI, aes(x=inputRate, y=moransI), log="y") + 
		theme_classic() + 
		geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6), color=cbPalette[1]) + 
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=10)) + 
		theme(plot.margin = unit(c(0.2, 0.2, 0.45, 0.45), "in")) + 
		theme(axis.title.y=element_text(hjust=0.5, size=26)) + 
		geom_vline(xintercept=c(5), col="grey", linetype="dashed") + theme(legend.position="none") + 
		theme(axis.title.x=element_blank()) + 
		theme(plot.margin=unit(c(0.25, 0.5, 1.5, 0.25), "lines")) + 
		theme(legend.position=c(0.8, 0.7), legend.key.size=unit(3, "lines"), legend.text=element_text(size=14), legend.title=element_text(size=18), legend.key.width=unit(0.75, "lines")) + 
		scale_colour_manual(values=cbPalette[2]) + 
		scale_y_log10(name="Moran's I", limits=c(0.3, 1.02), breaks=lab_seq_lowMoransI, labels = trans_format("log10", math_format(10^.x))) + 
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5), axis.text=element_text(size = 16)) +
		annotate("text",x=1,y=0.9*max(lowI$moransI), vjust=-0.2, hjust=.2,label="E", size=12)

	lab_seq_highMoransI = 10^(seq(-1, 0, 0.25))
	highI_moransI = ggplot(highI, aes(x=inputRate, y=moransI), log="y") + 
		theme_classic() + 
		geom_boxplot(width=0.5, lwd=1.25, fatten=1.5, position=position_dodge(.6), color=cbPalette[1]) + 
		theme(plot.title = element_text(hjust = 0.5, size=22), axis.text=element_text(size=10)) + 
		theme(plot.margin = unit(c(0.2, 0.2, 0.45, 0.45), "in")) + 
		theme(axis.title.y=element_text(hjust=0.5, size=26)) + 
		geom_vline(xintercept=c(5), col="grey", linetype="dashed") + theme(legend.position="none") + 
		theme(axis.title.x=element_blank()) + 
		theme(plot.margin=unit(c(0.25, 0.5, 1.5, 0.25), "lines")) + 
		theme(legend.position=c(0.8, 0.7), legend.key.size=unit(3, "lines"), legend.text=element_text(size=14), legend.title=element_text(size=18), legend.key.width=unit(0.75, "lines")) + 
		scale_colour_manual(values=cbPalette[2]) + 
		scale_y_log10(name="Moran's I", limits=c(0.2, 1.02), breaks=lab_seq_highMoransI, labels = trans_format("log10", math_format(10^.x))) + 
		theme(axis.text.y = element_text(angle = 90, hjust = 0.5), axis.text=element_text(size = 16)) +
		annotate("text",x=1,y=0.8*max(highI$moransI), vjust=-0.2, hjust=.2,label="F", size=12)

	jpeg(file.path(".", "Figures", "Figure4.jpg"), width=1200, height=1350)
	panels = plot_grid(lowI_sd, highI_sd, lowI_ACd, highI_ACd, lowI_moransI, highI_moransI, ncol=2, align="vh")
	ggdraw(add_sub(panels, "Phosphorus Input Rate (i)", vpadding=grid::unit(.25,"lines"),y=0.5, x=0.5, vjust=0, size=24))
	dev.off()

}