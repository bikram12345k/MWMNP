## Multinomial probit model with 3 choices

## Notation
## n number of customers, indexed by i
## k = 3 choices (say), indexed by j
## p is the number of exogeneous covariates

## the probit outcome is yij
## the observed outcome is choicei taking values in 1:k

## covariate is Xij
## regression coefficient b
## Assume first that the cov matrix is I
#set.seed(0841)
### Simulation model


#args = commandArgs(trailingOnly=TRUE)
#arrayID=as.numeric(args)[1]

library(truncnorm)
library(progress)

if(!exists('nInstances')) nInstances = 10
## Keep a progress bar
pb <- progress_bar$new(total = 100)

for(sampItr in 1:nInstances){
	pb$tick()
	cat("\n")

	if(!exists('n'))
		n = 1000
	if(!exists('k'))
		k = 3
	if(!exists('filename'))
		filename = paste0('runtimes_n_',n,'_k_',k,'.csv')

	k1 = k
	p = 2
	X = matrix(runif(n*k*p, 0, 1), n*k, p)
	## Use different coefficients based on zip codes
	zip1 = c(rep(1, n/2), rep(2, n/2))	# Use these on the betas
	zip2 = sample(zip1)	# Use these on the alphas

	# Create new X matrix including the preference dummies
	X = cbind(diag(rep(1, k1))[rep(1:k1,times=n),], X)
	
	a1 = matrix(0 + seq(0, k1-1)*.25, ncol=k1, nrow=n, byrow=TRUE)
	a2 = matrix(0 + seq(k1-1, 0)*.25, ncol=k1, nrow=n, byrow=TRUE)
	a = matrix(0, n, k1)
	a[zip1==1,] = a1[zip1==1,]
	a[zip1==2,] = a2[zip1==2,]
	a = a[rep(1:n,each=k),]
	
	bet = 1*(zip2==1) + .5*(zip2==2)
	gam = -.5*(zip2==1) - 1.5*(zip2==2)##

	b = cbind(rep(bet, each=k), rep(gam, each=k))
		
	ab = cbind(a, b)
	## Generate outcome
	y = rnorm(n*k, m=rowSums(X*ab), 1)
	choice = sapply(1:n, function(i) which.max(y[(i-1)*k+(1:k)]))


	#################################
	########## Monte Carlo EM #######

	tempt = Sys.time()

	####### Initialization
	b0_1 = runif(p,-1,1)
	b0_2 = runif(p,-1,1)
	b0 = matrix(0, n, p)
	b0[zip2==1,] = b0_1
	b0[zip2==2,] = b0_2
			
	a0_1 = runif(k,-1,1)
	a0_2 = runif(k,-1,1)
	a0 = matrix(0, n, k)
	a0[zip2==1,] = a0_1
	a0[zip2==2,] = a0_2
	
	ab0 = cbind(a0,b0)
	
	sigma20 = runif(k, .5, 1)
	S0 = diag(sigma20)

	emItr = 0
	emITR = 5000
	
	while(1){
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

		for(backfitItr in 1:100){
			E_gamma = E - rowSums(X[,-(1:k)]*b1[rep(1:n, each=k),]) #b0[,2]
			# Step 1
			XtSigmaX_bet = matrix(0, k1, k1)
			XtSigmay_bet = matrix(0, k1, 1)
			for(i in which(zip1==1)){
				XtSigmaX_bet = XtSigmaX_bet + 
						t(X[(i-1)*k + (1:k),1:k,drop=FALSE])%*%solve(S0)%*% X[(i-1)*k + (1:k),1:k1,drop=FALSE]
				XtSigmay_bet = XtSigmay_bet + t(X[(i-1)*k + (1:k),1:k1,drop=FALSE])%*%solve(S0)%*%E_gamma[(i-1)*k+(1:k)]
			}
			a1_1 = solve(XtSigmaX_bet)%*%XtSigmay_bet

			XtSigmaX_bet = matrix(0, k, k)
			XtSigmay_bet = matrix(0, k, 1)
			for(i in which(zip1==2)){
				XtSigmaX_bet = XtSigmaX_bet + 
						t(X[(i-1)*k + (1:k),1:k1,drop=FALSE])%*%solve(S0)%*% X[(i-1)*k + (1:k),1:k1,drop=FALSE]
				XtSigmay_bet = XtSigmay_bet + t(X[(i-1)*k + (1:k),1:k1,drop=FALSE])%*%solve(S0)%*%E_gamma[(i-1)*k+(1:k)]
			}
			a1_2 = solve(XtSigmaX_bet)%*%XtSigmay_bet

			a1 = matrix(0, n, k1)
			a1[zip1==1,] = a1_1
			a1[zip1==2,] = a1_2
			#c(bet1_1)*(zip1 == 1) + c(bet1_2)*(zip1 == 2)

			# Step 2
			E_bet = E - rowSums(X[,1:k1]*a1[rep(1:n, each = k),])

			XtSigmaX_gam = matrix(0, p, p)
			XtSigmay_gam = matrix(0, p, 1)
			for(i in which(zip2==1)){
				XtSigmaX_gam = XtSigmaX_gam + 
						t(X[(i-1)*k + (1:k),-(1:k1),drop=FALSE])%*%solve(S0)%*% X[(i-1)*k + (1:k),-(1:k1),drop=FALSE]
				XtSigmay_gam = XtSigmay_gam + t(X[(i-1)*k + (1:k),-(1:k1),drop=FALSE])%*%solve(S0)%*%E_bet[(i-1)*k+(1:k)]
			}
			b1_1 = solve(XtSigmaX_gam)%*%XtSigmay_gam

			XtSigmaX_gam = matrix(0, p, p)
			XtSigmay_gam = matrix(0, p, 1)
			for(i in which(zip2==2)){
				XtSigmaX_gam = XtSigmaX_gam + 
						t(X[(i-1)*k + (1:k),-(1:k),drop=FALSE])%*%solve(S0)%*% X[(i-1)*k + (1:k),-(1:k),drop=FALSE]
				XtSigmay_gam = XtSigmay_gam + t(X[(i-1)*k + (1:k),-(1:k),drop=FALSE])%*%solve(S0)%*%E_bet[(i-1)*k+(1:k)]
			}
			b1_2 = solve(XtSigmaX_gam)%*%XtSigmay_gam

			b1 = matrix(0, n, p)
			b1[zip2==1,] = b1_1
			b1[zip2==2,] = b1_2
			#b1 = c(gam1_1)*(zip2 == 1) + c(gam1_2)*(zip2 == 2)
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
		err = mean(abs(c(a0_1-a0_1[1], a0_2-a0_1[1], b0_1, b0_2)/sqrt(S0[1,1])-
					c(a1_1-a1_1[1], a1_2-a1_1[1], b1_1, b1_2)/sqrt(S1[1,1])))
					
		#print( c(round(c(emItr, NA, a1_1-a1_1[1], a1_2-a1_1[1], NA, b1_1, b1_2, NA, diag(S1)),3), round(err,4)) )	
			
		if( err < 0.005 ) break;
		
		if(emItr %% 10 == 0){
			cat("sampItr: = ", sampItr, "\temItr: = ",emItr,"\n")
			print( c(round(c(emItr, NA, a1_1-a1_1[1], a1_2-a1_1[1], NA, b1_1, b1_2, NA, diag(S1)),3), round(err,4)) )
		}
			
		if( emItr >= emITR ) break;
		
		
		a0_1 = a1_1
		a0_2 = a1_2
		b0_1 = b1_1
		b0_2 = b1_2
		
		ab0 = ab1
		S0 = S1
	}

	tt = Sys.time() - tempt

	
	res = c(sampItr,  tt, attr(tt,'units'), emItr,
			err, c(a1_1-a1_1[1], a1_2-a1_1[1], b1_1, b1_2, diag(S1)))
	res = matrix(res, nrow=1)
	colnames(res) = c('sampItr', 'time2converge', 'timeunits', 'noItr2converge',
				'error', paste0('alpha_zip1_',1:k),	paste0('alpha_zip2_',1:k),	
			paste0('beta_zip1_',1:p), paste0('beta_zip2_',1:p), 
		paste0('sigma2_',1:k))
	
	write.table(res, file=filename, sep = ",", row.names=FALSE, col.names=(sampItr==1), 
			append = (sampItr!=1))
}
