#calculate the partial differential equations of the state variables
calc_partialDerivs <- function(nutrients, phytos, Dx=dx, Dy=dy){
	
	# expand nutrients for periodic boundaries
	nutrients_expand = matrixExpand_periodicBounds(nutrients)
	# expand phytos for periodic boundaries
	phytos_expand = matrixExpand_periodicBounds(phytos)

	# shift grids for calculating derivatives
	n_expand_shiftLeft = cbind(nutrients_expand[,-1], rep(NA, nrow(nutrients_expand)))
	n_expand_shiftRight = cbind(rep(NA, nrow(nutrients_expand)), nutrients_expand[,-ncol(nutrients_expand)])
	n_expand_shiftUp = rbind(nutrients_expand[-1,], rep(NA, ncol(nutrients_expand)))
	n_expand_shiftDown = rbind(rep(NA, ncol(nutrients_expand)), nutrients_expand[-nrow(nutrients_expand), ])

	# shift grids for calculating derivatives
	p_expand_shiftLeft = cbind(phytos_expand[,-1], rep(NA, nrow(phytos_expand)))
	p_expand_shiftRight = cbind(rep(NA, nrow(phytos_expand)), phytos_expand[,-ncol(phytos_expand)])
	p_expand_shiftUp = rbind(phytos_expand[-1,], rep(NA, ncol(phytos_expand)))
	p_expand_shiftDown = rbind(rep(NA, ncol(phytos_expand)), phytos_expand[-nrow(phytos_expand), ])

	# calc first derivatives
	dNutrients_dX = (n_expand_shiftLeft - n_expand_shiftRight) / (2*Dx)
	dNutrients_dY = (n_expand_shiftDown - n_expand_shiftUp) / (2*Dy)
	dPhytos_dX = (p_expand_shiftLeft - p_expand_shiftRight) / (2*Dx)
	dPhytos_dY = (p_expand_shiftDown - p_expand_shiftUp) / (2*Dy)

	# calc second derivatives
	d2N_dX2 = (n_expand_shiftLeft - 2*nutrients_expand + n_expand_shiftRight) / (Dx * Dx)
	d2N_dY2 = (n_expand_shiftDown - 2*nutrients_expand + n_expand_shiftUp) / (Dy * Dy)
	d2P_dX2 = (p_expand_shiftLeft - 2*phytos_expand + p_expand_shiftRight) / (Dx * Dx)
	d2P_dY2 = (p_expand_shiftDown - 2*phytos_expand + p_expand_shiftUp) / (Dy * Dy)

	# bundle up state variable and first and second partial derivatives	into array
	Dims = dim(nutrients)
	system_out = array(NA, c(Dims, 2, 5), dimnames=list(NULL, NULL, c("nutrients", "phytos"), c("stateVar", "dVar_dx", "dVar_dy", "d2Var_dx2", "d2Var_dy2")))
	system_out[ , , "nutrients", "stateVar"] = nutrients
	system_out[ , , "phytos", "stateVar"] = phytos

	system_out[ , , "nutrients", "dVar_dx"] = matrixTrim_outerCells(dNutrients_dX)
	system_out[ , , "phytos", "dVar_dx"] = matrixTrim_outerCells(dPhytos_dX)

	system_out[ , , "nutrients", "dVar_dy"] = matrixTrim_outerCells(dNutrients_dY)
	system_out[ , , "phytos", "dVar_dy"] = matrixTrim_outerCells(dPhytos_dY)

	system_out[ , , "nutrients", "d2Var_dx2"] = matrixTrim_outerCells(d2N_dX2)
	system_out[ , , "phytos", "d2Var_dx2"] = matrixTrim_outerCells(d2P_dX2)

	system_out[ , , "nutrients", "d2Var_dy2"] = matrixTrim_outerCells(d2N_dY2)
	system_out[ , , "phytos", "d2Var_dy2"] = matrixTrim_outerCells(d2P_dY2)

	return(system_out)
}