# function to set up advection field using randomly seeded eddies

makeEddy <- function(eddyN=100, eddySeed=2017, maxVel= 0.3, R=10){
	#set up data storage matices
	d_psi_components = array(data=0, dim=c(numberOfGridCells+1, numberOfGridCells+1, 2)) 
	vx_partialDiffs = array(data=0, dim=c(numberOfGridCells+1, numberOfGridCells+1, 2))
	vy_partialDiffs = array(data=0, dim=c(numberOfGridCells+1, numberOfGridCells+1, 2))
	vel_mag = array(data=0, dim=c(numberOfGridCells+1, numberOfGridCells+1))
	# #generate the random numbers for all eddies to start
	set.seed(eddySeed)
	randoms_x = runif(eddyN)
	randoms_y = runif(eddyN)
	# #iterate through each eddy
	for(n in 1:eddyN){
		# turn random numbers into points on the grid
		eddyCenter_x = ((xmax-xmin)*randoms_x[n]+xmin)
		eddyCenter_y = ((ymax-ymin)*randoms_y[n]+ymin)
		#iterate through each sub-grid of 9x9 sub-grids, with center sub-grid being "focal grid" for which simulation defined on
	 	delta_x = grid_x - eddyCenter_x
	 	delta_y = grid_y - eddyCenter_y
	 	x_offset = c(-1,0,1)
	 	y_offset = c(-1,0,1)
	 	for(i in 1:length(x_offset)){
	 		for(j in 1:length(y_offset)){
	 			cur_delta = array(data=0, dim=c(numberOfGridCells+1, numberOfGridCells+1, 2)) 
	 			cur_delta[,,1] = delta_x + x_offset[i]*(xmax-xmin) # 1 is index for x
	 			cur_delta[,,2] = delta_y + y_offset[j]*(ymax-ymin) # 2 is index for y
	 			f_cur = exp(-1*(cur_delta[,,1]^2 + cur_delta[,,2]^2)/(2*R^2)  ) 
	 			d_psi_components[,,1] = d_psi_components[,,1] + ((-1)^n)*f_cur*cur_delta[,,2] #d_psi_components[,,1] is for velocity in x direction
	 			d_psi_components[,,2] = d_psi_components[,,2] - ((-1)^n)*f_cur*cur_delta[,,1] #d_psi_components[,,2] is for velocity in x direction
	 		}
	 	}
	}
	v_x = d_psi_components[,,1]
	v_y = d_psi_components[,,2]

	# expand
	v_x_expand = matrixExpand_periodicBounds(v_x)
	v_y_expand = matrixExpand_periodicBounds(v_y)
	# shift matrices
	v_x_shiftLeft = matrix_shift(v_x_expand, shift = "left")
	v_x_shiftRight = matrix_shift(v_x_expand, shift = "right")
	v_y_shiftUp = matrix_shift(v_y_expand, shift = "up")
	v_y_shiftDown = matrix_shift(v_y_expand, shift = "down")
	# calc derivatives
	dVx_dx_expand = (v_x_shiftLeft - v_x_shiftRight) / (2*dx)
	dVy_dy_expand = (v_y_shiftDown - v_y_shiftUp) / (2*dy)

 	# determine scaling to be applied
	v_mag = (v_x^2 + v_y^2)^0.5
	v_orig_max = max(v_mag)
	scaling = maxVel / v_orig_max

	#bundle needed components for outup
	velocity_field_out = array(NA, c(numberOfGridCells+1, numberOfGridCells+1, 5), dimnames=list(NULL, NULL, c("v_x", "v_y", "dVx_dx", "dVy_dy", "vel_magnitude")))
	velocity_field_out[ , , "v_x"] = v_x * scaling
	velocity_field_out[ , , "v_y"] = v_y * scaling
	velocity_field_out[ , , "dVx_dx"] = matrixTrim_outerCells(dVx_dx_expand) * scaling
	velocity_field_out[ , , "dVy_dy"] =  matrixTrim_outerCells(dVy_dy_expand) * scaling
	velocity_field_out[ , , "vel_magnitude"] = (velocity_field_out[ , , "v_x"]^2 + velocity_field_out[ , , "v_y"]^2)^0.5
	return(velocity_field_out)
}