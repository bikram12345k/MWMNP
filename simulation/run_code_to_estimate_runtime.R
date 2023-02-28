# choose your favorite seed for replicability
library(truncnorm)
library(progress)


nInstances = 10

for(n in c(1000,	2000,	5000))
	for(k in c(3, 5, 10)){
	
		set.seed(0841)
		
		print(c(n, k))
		
		filename = paste0('runtimes_n_',n,'_k_',k,'.csv')
	
		source('gwr_backfitting_simulation_code.R')

	}
#n = 	#1000	2000	5000
#k = 	#3	5	10

# The result will be in a file named 
filename
