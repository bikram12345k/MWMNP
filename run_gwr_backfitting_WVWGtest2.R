library(parallel)
library(doSNOW)
library(foreach)
library(bayesm)
library(biglm)
library(truncnorm)
library(bayesm)

source('RcppSupportv2_new.R')

source("prepare_data_for_gwr_backfitting_orderfixed_SAC.R")


## SPECIFY weight matrices
Wpreference = WV		## MPG based weight matrix
Wresponse = WG		## Geography based weight matrix

## Filename of the file where the output will be written
filename = "Results_SAC/Result_latlong_WV_WG_orderfixed_SAC_nolatlong_gaussiankernel.txt"

print("Wpreference = WV, Wresponse = WG")
print("WV_WG_orderfixed_SAC_nogovtrebate_nolatlong_gaussiankernel")

## Run model fitting
source("gwr_backfitting_simulation_code_for_data.R")

