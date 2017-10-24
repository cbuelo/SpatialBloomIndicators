# functions for initialization of spatial algae and phytos distribution

getFixedPoint <- function(inputRate=inputRate, A=a, M=m, F=f){
	fixedPoint = numeric(2)
	Coef_n = (A*F - A - inputRate + M)/M
	Coef = (A*F-inputRate)/M
	nut_conc = quatratic_solve(Coef_n, Coef)
	fixedPoint[1] = nut_conc
	fixedPoint[2] = F/nut_conc+F-1
	return(fixedPoint)
}

quatratic_solve <- function(Coef_n, Coef){
	discrim = Coef_n^2 - 4*Coef # a = 0; see Serizawa 2008 equation 15
	n1 = (-1*Coef_n+discrim^0.5)/2
	n2 = (-1*Coef_n-discrim^0.5)/2
	if(n1 < n2){ #only want smaller root; for parameters in these analyses larger root makes phytos equilibrium concentration negative
		n_out = n1
	}else{
		n_out = n2
	}
	return(n_out)
}


# initialize the system
initializeSerizawa_nutrients <- function(x, y, numberOfGridCells, fixedPoint){
		xmax = max(x)
		xmin = min(x)
		hold = (xmax/xmax - xmin/xmax)
		out = fixedPoint[1]*(1+sin(pi*(((hold - (1/((numberOfGridCells+1)/2))) / hold) * (x/xmax - xmin/xmax) + (xmin / xmax))))
		return(out)
}

initializeSerizawa_phytos <- function(x, y, numberOfGridCells, fixedPoint){
		ymax = max(y)
		ymin = min(y)
		hold = (ymax/ymax - ymin/ymax)
		out = fixedPoint[2]*(1+sin(pi*(((hold - (1/((numberOfGridCells+1)/2))) / hold) * (y/ymax - ymin/ymax) + (ymin / ymax))))
		return(out)
}