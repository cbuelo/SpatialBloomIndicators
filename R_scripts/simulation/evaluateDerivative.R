evalDeriv <- function(systemState, velocityField, I=inputRate, A=a, M=m, Dp=DP, Dn=DN, F=f, gridN=numberOfGridCells){
	# calculate derivative for nutrients
	dN_dt_diffusion = Dn*(systemState[ , , "nutrients", "d2Var_dx2"] + systemState[ , , "nutrients", "d2Var_dy2"])
	dN_dt_biology = I - A * (systemState[ , , "nutrients", "stateVar"] / (1 + systemState[ , , "nutrients", "stateVar"]))*systemState[ , , "phytos", "stateVar"] - M*systemState[ , , "nutrients", "stateVar"]
	dN_dt_advection = velocityField[ , , "v_x"] * systemState[ , , "nutrients", "dVar_dx"] + velocityField[ , , "dVx_dx"] * systemState[ , , "nutrients", "stateVar"] +  velocityField[ , , "v_y"] * systemState[ , , "nutrients", "dVar_dy"] + velocityField[ , , "dVy_dy"] * systemState[ , , "nutrients", "stateVar"]

	dN_dt_total = dN_dt_diffusion + dN_dt_biology + dN_dt_advection

	#calculate derivative for phytos
	dP_dt_diffusion = Dp*(systemState[ , , "phytos", "d2Var_dx2"] + systemState[ , , "phytos", "d2Var_dy2"])
	dP_dt_biology = (systemState[ , , "nutrients", "stateVar"] / (1 + systemState[ , , "nutrients", "stateVar"])) * systemState[ , , "phytos", "stateVar"] - F * (systemState[ , , "phytos", "stateVar"] / (1 + systemState[ , , "phytos", "stateVar"]))
	dP_dt_advection = velocityField[ , , "v_x"] * systemState[ , , "phytos", "dVar_dx"] + velocityField[ , , "dVx_dx"] * systemState[ , , "phytos", "stateVar"] +  velocityField[ , , "v_y"] * systemState[ , , "phytos", "dVar_dy"] + velocityField[ , , "dVy_dy"] * systemState[ , , "phytos", "stateVar"]

	dP_dt_total = dP_dt_diffusion + dP_dt_biology + dP_dt_advection

	#return
	deriv_out = array(NA, c(gridN + 1, gridN + 1, 8), dimnames=list(NULL, NULL, c("dN_dt_diffusion", "dN_dt_biology", "dN_dt_advection", "dN_dt", "dP_dt_diffusion", "dP_dt_biology", "dP_dt_advection", "dP_dt")))
	deriv_out[ , , "dN_dt_diffusion"] = dN_dt_diffusion
	deriv_out[ , , "dN_dt_biology"] = dN_dt_biology
	deriv_out[ , , "dN_dt_advection"] = dN_dt_advection
	deriv_out[ , , "dN_dt"] = dN_dt_total
	deriv_out[ , , "dP_dt_diffusion"] = dP_dt_diffusion
	deriv_out[ , , "dP_dt_biology"] = dP_dt_biology
	deriv_out[ , , "dP_dt_advection"] = dP_dt_advection
	deriv_out[ , , "dP_dt"] = dP_dt_total

	return(deriv_out)
}