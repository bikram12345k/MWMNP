	n = 1000
	k = 3

for(n in c(1000, 2000, 5000)){
	for(k in c(3,5,10)){

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

	choicemodel = NULL
	for(i in 1:2000){
		y = rnorm(n*k, m=rowSums(X*ab), 1)
		choice = sapply(1:n, function(i) which.max(y[(i-1)*k+(1:k)]))
		choicemodel = cbind(choicemodel, choice)
	}

	rowMeans(choicemodel)


	res = read.csv(paste0('runtimes_n_',n,'_k_',k,'.csv'))

	sse = NULL
	sse_pr = NULL
	for(r in 1:nrow(res)){
		a1 = matrix(0 + as.numeric(res[r,6:(6+k-1)]), ncol=k1, nrow=n, byrow=TRUE)
		a2 = matrix(0 + as.numeric(res[r,(6+k):(6+2*k-1)]), ncol=k1, nrow=n, byrow=TRUE)
		a = matrix(0, n, k1)
		a[zip1==1,] = a1[zip1==1,]
		a[zip1==2,] = a2[zip1==2,]
		a = a[rep(1:n,each=k),]
		
		bet = res[r,(6+2*k)]*(zip2==1) + res[r,(6+2*k+2)]*(zip2==2)
		gam = res[r,(6+2*k+1)]*(zip2==1) + res[r,(6+2*k+3)]*(zip2==2)##


		b = cbind(rep(bet, each=k), rep(gam, each=k))
			
		ab = cbind(a, b)
		## Generate outcome

		choicesim = NULL
		for(i in 1:2000){
			y = rnorm(n*k, m=rowSums(X*ab), s=rep(sqrt(as.numeric(res[r,(6+2*k+4:6)])), n)  )
			choice = sapply(1:n, function(i) which.max(y[(i-1)*k+(1:k)]))
			choicesim = cbind(choicesim, choice)
		}


		sse = c(sse, sum( abs(rowMeans(choicemodel) - rowMeans(choicesim)) ))

		sse_pr_r = NULL
		for(i in 1:n){

			tab1_temp = table(choicemodel[i,])/2000
			tab2_temp = table(choicesim[i,])/2000
			tab1 = tab2 = rep(0, k)
			tab1[as.numeric(names(tab1_temp))] = tab1_temp
			tab2[as.numeric(names(tab2_temp))] = tab2_temp
			sse_pr_r[i] = sum(abs(tab1 - tab2))/k
		}
		sse_pr = c(sse_pr, sum(sse_pr_r))

	}

	print( c(n, k, mean(sse), mean(sse_pr), mean(res[,4])) )

}
}



for(n in c(1000, 2000, 5000)){
	for(k in c(3,5,10)){

	res = read.csv(paste0('runtimes_n_',n,'_k_',k,'.csv'))
	print( c(n, k, mean(res[,2]), res[1,3]) )

}
}

	
