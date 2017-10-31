# SpatialBloomIndicators
A project testing if spatial resilience statistics can provide information on an aquatic ecosystem's bloom state and proximity to thresholds. 

## Details
This project implements the spatial algal bloom model from Serizawa et al. 2008 (1) in R. The model is defined on a two-dimensional grid and has dynamic components for both nutrients and phytoplankton. Grid cells exchange material via the processes of diffusion and advection, the latter determined from a velocity field created by randomly-seeded eddies.

The "R_scripts" directory contains functions for model simulation (deterministic and stochastic) as well as analysis of the resulting outputs. Within this directory, the "runAll.R" script carries out a reproducible set of simulations and analyses and generates figures for a submitted manuscript documenting our findings.

## References
1. Serizawa, H, T Amemiya, K Itoh. 2008. Patchiness in a minimal nutrient-phytoplankton model. Journal of Biosciences, 33(3): 391-403. [https://doi.org/10.1007/s12038-008-0059-y](https://doi.org/10.1007/s12038-008-0059-y)