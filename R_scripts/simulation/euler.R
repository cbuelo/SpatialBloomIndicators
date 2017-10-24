#function to integrate using euler method for spatial algal bloom model
euler <- function(sysState_start, velField, dT = stepSize, I=inputRate, A=a, M=m, Dp=DP, Dn=DN, F=f, gridN=numberOfGridCells, Dx=dx, Dy=dy, stochastic=stochastic, sigma=sigma){
		deriv = evalDeriv(systemState = sysState_start, velocityField = velField, Dn=Dn, I=I, A=A, M=M, Dp=Dp, F=F, gridN=gridN)

		sysState_hold = sysState_start[ , , c("nutrients", "phytos"), "stateVar"] + deriv[ , ,c("dN_dt", "dP_dt")]  * dT

		if(stochastic == TRUE){
			randNums = matrix(rnorm(((gridN+1)*(gridN+1))), gridN+1, gridN+1)
			addedNoise = sysState_hold[ , , "phytos"] * sigma * randNums * sqrt(dT)
			
			sysState_hold[ , , "phytos"] = sysState_hold[ , , "phytos"] + addedNoise
		}
		sysState_next = calc_partialDerivs(nutrients = sysState_hold[ , ,"nutrients"], phytos = sysState_hold[ , , "phytos"], Dx=Dx, Dy=Dy)

		return(sysState_next)
}