## Multinomial probit model with 3 choices

## Notation
## n number of customers, indexed by i
## k = 3 choices (say), indexed by j
## p is the number of exogeneous covariates

## the probit outcome is yij
## the observed outcome is choicei taking values in 1:k




#################################
########## Monte Carlo EM #######

tempt = Sys.time()


## Specify the model variables
choice = y
p = ncol(X) - k
initiate = 1

ncores = 24 #detectCores()
#detach("package:parallel")

ab0 = matrix(runif(n*p1,-.5, .5), n, p1)

sigma20 = runif(k, .5, 1)
S0 = diag(sigma20)

emItr = 0
emITR = 1000
initiate = 1
keep = 10

while(1){

	if(initiate ==1){
		## Open a cluster, it will run for 'keep' (=10) many iterations.
		cl<-makeCluster(ncores -3) #change the 2 to your number of CPU cores
		registerDoSNOW(cl)
		initiate = 0
	}


	####### E step
	### Monte Carlo/Gibbs E step
	emItr = 1 + emItr
	## Run a Gibbs  step
	y0 = runif(n*k, -1, 1)	#initialize the utilities
	y1 = y0
	ITR = 300
	nburn = 50
	thin = 8
	
	
	ab0 = ab0[rep(1:n, each=k),]

	yGibbs = NULL

	for(gibbsitr in 1:ITR){


		for(j in 1:k){
			# Calculate conditional mean and variance of j given -j (or mj)
			idxj = seq(j, n*k, by=k)
			y1mj = matrix(y1[-idxj] - (rowSums(X*ab0))[-idxj], k-1, n)

			sigmaj_mj = S0[j,-j,drop=FALSE] 
			sigmamj_mj = S0[-j,-j,drop=FALSE]
			sigmaj_j = S0[j,j,drop=FALSE]

			temp = sigmaj_mj%*%solve(sigmamj_mj)%*%(y1mj)	## 1*n matrix

			muj_givenmj = (rowSums(X*ab0))[idxj] + as.numeric(temp)
			varj_givenmj = sigmaj_j - sigmaj_mj%*%solve(sigmamj_mj)%*%t(sigmaj_mj)
			
			# simulate the values
			# install.packages('truncnorm')
			# library(truncnorm)
			loweruppertrunc = sapply(1:n, function(i){
							if(choice[i] == j) return(c(max(y1[(i-1)*k+(1:k)][-j]), Inf))
							if(choice[i] != j) return(c(-Inf, max(y1[(i-1)*k+(1:k)][-j])))
							})

			y1[idxj] = rtruncnorm(n, a = loweruppertrunc[1,], b = loweruppertrunc[2,], 
						mean = muj_givenmj, sd = sqrt(varj_givenmj))


		}

		yGibbs = rbind(yGibbs, y1)
		
	}


	## Calculate the Expectation and the variance
	E = colMeans(yGibbs[seq(nburn + 1, ITR, thin),])
	sigmaj = apply(yGibbs[seq(nburn + 1, ITR, thin),], 2, 
					function(x) sum((x - mean(x))^2)/(length(x))
					)

	####### M step
	### Two steps
	##	Maximization over b/beta
	#######################################
	# Use backfitting
	#######################################
	b1 = matrix(0,n,p)
	# gam1 = rep(0, n)

	

	## Calculation using the W matrices		

	for(backfitItr in 1:15){
		E_gamma = E - rowSums(X[,-(1:k)]*b1[rep(1:n, each=k),]) #b0[,2]

	
		# Step 1

		Xa = X[,1:k,drop=FALSE]
		a1 = matrix(0, n, k1)
		#Fix i
		a1 = foreach(i=1:n, .combine = rbind, .packages='biglm') %dopar% {

			temp = data.frame(E_gamma, Xa, w = sqrt(Wpreference[i,rep(1:n,each=k)])*rep(1/sqrt(diag(S0)), n))

			res = biglm(as.formula(paste0("E_gamma ~ -1 + ", paste0("X",1:k, collapse="+"))), 
						weights = asOneSidedFormula("w"), 
						data=temp)

			#a1[i,] = 
			summary(res)$mat[,1]			
		}

		
		# Step 2

		E_bet = E - rowSums(X[,1:k1]*a1[rep(1:n, each = k),])

		Xb = X[,-(1:k),drop=FALSE]
		b1 = matrix(0, n, p)
		#Fix i
		b1 = foreach(i=1:n, .combine = rbind, .packages='biglm') %dopar% {

			temp = data.frame(E_bet, Xb, w = sqrt(Wresponse[i,rep(1:n,each=k)])*rep(1/sqrt(diag(S0)), n))

			res = biglm(as.formula(paste0("E_bet ~ -1 + ", paste0("X",1:p, collapse="+"))), 
						weights = asOneSidedFormula("w"), 
						data=temp)

			summary(res)$mat[,1]			
		}

	}


	## 	Maximization over Omega/Sigma
	## Assume that Omega/Sigma is diagonal
	## formula  (1/n)sum_i ( var(yij) + E(yij - Xijb1)^2 )
	ab1 = cbind(a1, b1)

	temp = NULL
	for(j in 1:k){
		o = sum(sigmaj[seq(j, n*k, by=k)])
		o = o + sum( (E[seq(j, n*k, by=k)] - rowSums(X[seq(j, n*k, by=k),]*ab1))^2 )
		o = (1/n)*o
		temp = c(temp, o)
	}

	S1 = diag( temp )
	#S1[1,1] = 1
	
	
	#write.table( c(	100*arrayID+ sampItr,
	#				emItr,
	#				mean(b1[zip1==1,1]),
	#				mean(b1[zip1==2,1]),
	#				mean(b1[zip2==1,2]),
	#				mean(b1[zip2==2,2]),
	#				diag(S1)),
	#			row.names = FALSE, col.names = FALSE, 
	#			file = paste0('gwr_backfitting_simpleprobit_p_',p,'_k_',k,'_n_',n,'_seed_',100*arrayID+ sampItr, '.txt'),
	#			append = TRUE, sep=',') 
	

	err = mean(abs(ab0[seq(1,nrow(ab0),by=k),] - ab1)/sqrt(S1[1,1]))

	#print( c(round(c(emItr, NA, a1_1-a1_1[1], a1_2-a1_1[1], NA, b1_1, b1_2, NA, diag(S1)),3), round(err,4)) )	
		
	if( err < 0.001 ) break;
	
	if(emItr %% keep == 0 | emItr == 1){
		#stopCluster(cl)
	
		
		## Calculate MAD
		## Create a new dataset of the differences
		
		madL = deprintize(llmnp3)(t(ab1), Sigmadiff(diag(S1), k), Xdiff , y, r=100)
		mad = madL$mad
		llike = madL$llike


		cat("emItr: = ",emItr,"\n")
		print( c(round(c(emItr, NA, colMeans(a1), NA, colMeans(b1), NA),3), round(err,4)) )
		cat("mad= ",mad,"\n\n")


		temps = Sys.time()
		cat("Time spent: ", round(temps - tempt, 2)," ", attr(temps-tempt,"units"),"\n\n")

	

		# what to write in res
		res = cbind(emItr, ab1)
		res = rbind(res, c(NA, diag(S1),rep(NA, p)))
		res = rbind(res, mad)
		res = rbind(res, llike)

		rownames(res) = c(1:n, "sigma2", "mad", "llike")
		colnames(res) = c("emItr", paste0("PrefC",1:k), paste0("RespC",1:p))

		deprintize(write.table)(res, file=filename, sep = ",", row.names=TRUE, col.names=(emItr == 1), 
				append = TRUE)

	}

	if( emItr >= emITR ) break;
	
	
	ab0 = ab1
	S0 = S1


}

