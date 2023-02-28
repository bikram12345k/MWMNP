## This document describes the use of the R codes for analysis of the paper.

### For the real data analysis. ###
## Transactions data set is referred to as 'target4.txt' in the code. 
##	This data set is NOT included with the codes. Section 2 of the paper
##	describes the content of the data set.
## MPG data set is referred to as 'MPG_unique.txt' in the codes.
##	This data set is included with the codes.
## 'RcppSupportv2_new.R' is supplementary code for some of the R functions
## 	used in the analysis.

## Steps to fitting MWMNP to the real data.
Run 'run_gwr_backfitting_WVWGtest2.R' to fit WVWG model as in Table 2 of the paper. This code calls 
	1) 'prepare_data_for_gwr_backfitting_orderfixed_SAC.R' to prepare the dataset, and then
	2) 'gwr_backfitting_simulation_code_for_data.R' to fit the model.
	
## Change the following two lines in 'run_gwr_backfitting_WVWGtest2.R' accordingly
to fit the other spatial models in Table 2.

Wpreference = WV		## MPG based weight matrix
Wresponse = WG		## Geography based weight matrix

## Standard error calculations are done in 'se_calculation.R'
This requires the result from the fit. Change the file name on line 189 of this
code to calculate standard errors of other spatial models.

## A template code for the price elasticity calculation is in 
'price_elasticity_WVWG.R'


### For the simulation results. ###
## Codes in the folder 'simulation/'

Run 'run_code_to_estimate_runtime.R' to run the simulations. This code calls the 
R code 'gwr_backfitting_simulation_code.R' to run the Monte Carlo EM algorithm
for MWMNP.

The simulation results are also included.
Table 7 of the paper, for simulations are calculated by 'calculate_simulation_results.R'
