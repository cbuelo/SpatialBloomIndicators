matrixExpand_periodicBounds <- function(X){
	X_expand_rowStart = rbind(X[nrow(X), ], X)
	X_expand_rowStartEnd = rbind(X_expand_rowStart, X[1,])
	X_expand_rowStartEnd_colStart = cbind(X_expand_rowStartEnd[,ncol(X_expand_rowStartEnd)], X_expand_rowStartEnd)
	X_expand_rowStartEnd_colStartEnd = cbind(X_expand_rowStartEnd_colStart, X_expand_rowStartEnd[,1])
	return(X_expand_rowStartEnd_colStartEnd)
}

matrixTrim_outerCells <- function(X){
	X_out = X[-c(1, nrow(X)), -c(1, ncol(X))]
	return(X_out)
}

matrix_shift <- function(X, shift){
	if(shift == "left"){
		return(cbind(X[,-1], rep(NA, nrow(X))))
	}
	if(shift == "right"){
		return(cbind(rep(NA, nrow(X)), X[,-ncol(X)]))
	}
	if(shift == "up"){
		return(rbind(X[-1,], rep(NA, ncol(X)) ))
	}
	if(shift == "down"){
		return(rbind(rep(NA, ncol(X)), X[-nrow(X), ]))
	}
}


plot_system <- function(system, derivs, T="", dt=""){
	dev.new(width=18, height=9)
	par(mfrow=c(2,5), oma = c(3.5, 3.5, 4, 4), mar=c(.5, .5, 4, 4))
	#plot nutrients
	image.plot(x=grid_x, y=grid_y, z=system[, , "nutrients", "stateVar"], main="Nutrients", xlab="", xaxt="n", yaxt="n")
	image.plot(x=grid_x, y=grid_y, z=derivs[, , "dN_dt"], main="dN/dt", xlab="", xaxt="n", yaxt="n")
	image.plot(x=grid_x, y=grid_y, z=derivs[, , "dN_dt_biology"], main="dN/dt bio", xlab="", xaxt="n", yaxt="n")
	image.plot(x=grid_x, y=grid_y, z=derivs[, , "dN_dt_advection"], main="dN/dt advection", xlab="", xaxt="n", yaxt="n")
	image.plot(x=grid_x, y=grid_y, z=derivs[, , "dN_dt_diffusion"], main="dN/dt diffusion", xlab="", xaxt="n", yaxt="n")
	#plot phytos
	image.plot(x=grid_x, y=grid_y, z=system[, , "phytos", "stateVar"], main="Phytos", xlab="", xaxt="n", yaxt="n")
	image.plot(x=grid_x, y=grid_y, z=derivs[, , "dP_dt"], main="dP/dt", xlab="", xaxt="n", yaxt="n")
	image.plot(x=grid_x, y=grid_y, z=derivs[, , "dP_dt_biology"], main="dP/dt bio", xlab="", xaxt="n", yaxt="n")
	image.plot(x=grid_x, y=grid_y, z=derivs[, , "dP_dt_advection"], main="dP/dt advection", xlab="", xaxt="n", yaxt="n")
	image.plot(x=grid_x, y=grid_y, z=derivs[, , "dP_dt_diffusion"], main="dP/dt diffusion", xlab="", xaxt="n", yaxt="n")
	#add legend
	mtext(paste("T = ", T,", dt = ", dt, sep=""), outer=TRUE, side=3)
}

plot_eddy <- function(Eddy, New=FALSE, background="none"){
	if(New==TRUE){
		dev.new()
	}
	plot(c(-100, 100, 100, -100), c(-100, -100, 100, 100))
	grid_width = 4
	if(background != "none"){
		image.plot(x=grid_x , y=grid_y , z=Eddy[,,background])
	}
		arrow.plot(a1 = as.vector(grid_x[seq(1, 181, grid_width),seq(1, 181, grid_width)]), a2 = as.vector(grid_y[seq(1, 181, grid_width),seq(1, 181, grid_width)]), u = as.vector(Eddy[seq(1, 181, grid_width),seq(1, 181, grid_width),"v_x"]), v=as.vector(Eddy[seq(1, 181, grid_width),seq(1, 181, grid_width),"v_y"]), arrow.ex=.03, length=0.04)
}