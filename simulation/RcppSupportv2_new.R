
##save workspace.
#sourceCpp("ghkvec_rcpp.cpp")

deprintize<-function(f){
 return(function(...) {capture.output(w<-f(...));return(w);});
}	
llmnp2 =
function (beta, Sigma, theta, X, y, r)
{
     pm1 = ncol(Sigma)
    k = length(beta)
#    mu = matrix(X %*% beta + theta, nrow = pm1)
    mu = matrix(X %*% beta/sqrt(Sigma[1,1]) + theta/sqrt(Sigma[1,1]), nrow = pm1)
    Sigma = Sigma/Sigma[1,1]
    logl = 0
    above = rep(0, pm1)
	Pj = c()
    for (j in 1:pm1) {
        muj = mu[, y == j]
        Aj = -diag(pm1)
        Aj[, j] = rep(1, pm1)
        trunpt = as.vector(-Aj %*% muj)
        Lj = t(chol(Aj %*% Sigma %*% t(Aj)))
		
		Pj = c(Pj, ghkvec(Lj, trunpt, above, r))
        #logl = logl + sum(log(Pj + 1e-50))
    }
    trunpt = as.vector(-mu[, y == (pm1 + 1)])
    Lj = t(chol(Sigma))
    above = rep(1, pm1)
	Pj = c(Pj, ghkvec(Lj, trunpt, above, r))
    #logl = logl + sum(log(ghkvec(Lj, trunpt, above, r) + 1e-50))
	logl = sum(log(Pj + 1e-50))
    return(list(mad=mean(1-Pj),llike=logl))
}


llmnp3 =
function (betamat, Sigma, X, y, r)
{
	#Sigma1 = 
    #pm1 = ncol(Sigma)
    k = nrow(betamat)
	n = ncol(betamat)
	pm1 = nrow(X)/n
	
#    mu = matrix(X %*% beta + theta, nrow = pm1)
	mu = sapply(1:n, function(i){
			X[(i-1)*pm1 + 1:pm1,] %*% betamat[,i]/sqrt(Sigma[1,1])
			})
    Sigma = Sigma/Sigma[1,1]
    logl = 0
    above = rep(0, pm1)
	Pj = c()
    for (j in 1:pm1) {
        muj = mu[, y == j]
        Aj = -diag(pm1)
        Aj[, j] = rep(1, pm1)
        trunpt = as.vector(-Aj %*% muj)
        Lj = t(chol(Aj %*% Sigma %*% t(Aj)))
		
		Pj = c(Pj, ghkvec(Lj, trunpt, above, r))
        #logl = logl + sum(log(Pj + 1e-50))
    }
    trunpt = as.vector(-mu[, y == (pm1 + 1)])
    Lj = t(chol(Sigma))
    above = rep(1, pm1)
	Pj = c(Pj, ghkvec(Lj, trunpt, above, r))
	print(Pj)
    #logl = logl + sum(log(ghkvec(Lj, trunpt, above, r) + 1e-50))
	logl = sum(log(sort(Pj)[-(1:16)] + 1e-50))
	#logl = logl - sum(log(sort(Pj)[1:16]+ 1e-50))
    #return(Pj)
	return(list(mad=mean(1-Pj),llike=logl))
}



llmnp4 =
function (betamat, Sigma, X, y, r)
{

    #pm1 = ncol(Sigma)
    k = nrow(betamat)
	n = ncol(betamat)
	pm1 = nrow(X)/n
	
    logl = 0
    above = rep(0, pm1)
	ll = c()
	for(i in 1:n){
		ll = c(ll, llmnp(beta = betamat[,i], Sigma = diag(Sigma[(i-1)*pm1+(1:pm1)]), 
					X = X[(i-1)*pm1 + (1:pm1), ], y = y[i], r = r))
	
	}
	logl = sum(ll) - sum(sort(ll)[1:5])
	#return(ll)
    return(list(mad=mean(1-exp(ll)),llike=logl))
}


################################################################################
### RCPP functions.   
library(Rcpp)
## Organize u vector

rcpp.organizeu <- 'NumericVector organizeu( NumericMatrix unoty, NumericVector uy, 
								IntegerVector y, int m, int p, int pm1 ){
	NumericVector out(m) ;
	int count = 0;
	
	for(int i = 0; i<y.size(); i++) {
	
		if(y[i] == p){
			for(int j=0; j<pm1; j++){
				out[count+j] = unoty(j,i);
			}
		}
		if(y[i] != p){
			for(int j=0; j<pm1; j++){
				if(j > y[i]-1)
					out[count+j] = unoty(j,i);
				if(j < y[i]-1)
					out[count+j] = unoty(j+1,i);
				if(j == y[i]-1)
					out[count+j] = uy[i];
			}
		}
				
		count += pm1;
	}
	
	return out;
}'
	
	
cppFunction(rcpp.organizeu)


rcpp.getresidual <- 'NumericMatrix getresidual(NumericVector u, NumericMatrix X, 
						NumericVector beta, int n, int pm1){
		NumericMatrix out(pm1, n);
		double countj = 0;
		
		for(int j=0; j<pm1; j++){
			
			for(int i=0; i<n; i++){
				countj = 0;
				for(int k=0; k<X.ncol(); k++)
					countj += X(j+pm1*i, k)*beta[k];
				out(j, i) = (u[j+pm1*i] - countj);
			}
		}
		
		return out;
	}'
	
cppFunction(rcpp.getresidual)

rcpp.getresidual1 <- 'NumericMatrix getresidual1(NumericVector u, NumericMatrix X, 
						NumericMatrix beta, int n, int pm1, int k){
		int nsq = n*n;
		NumericMatrix out(pm1, nsq);
		double count = 0;
		int npm1 = n*pm1;
		
		for(int i1=0; i1<n; i1++){
			for(int i2=0; i2<n; i2++)
				for(int j=0; j<pm1; j++){
					    count = 0;
						for(int k1 =0; k1<k; k1++)
							count += X(i1*npm1 + j+i2*pm1, k1)*beta(i1, k1);
					out(j, i1*n+i2) = u(i1*npm1 + j+i2*pm1) - count;
				}
		}			
					
					
		return out;
	}'
	
cppFunction(rcpp.getresidual1)

rcpp.organizetheta <- 'NumericVector organizetheta(NumericVector theta, int pm1, 
							int n){
	NumericVector out(theta);
	
	for(int i=0; i<pm1; i++)
		for(int j=0; j<n; j++)
			out[ ] = theta
	
}'

rcpp.append2X <- 'NumericMatrix append2X(NumericMatrix X, int pm1){
	NumericMatrix out(X.nrow(), X.ncol()+pm1);
	
	for(int i=0; i<out.nrow(); i++){
		for(int j=0; j<out.ncol(); j++){
			out(i,j) = 0;
			if(j<pm1){
				if((i-j) % pm1 == 0 ) 
					out(i,j) = 1;
			}
			if(j>=pm1)
				out(i,j) = X(i, j-pm1);
		}
	}
	
	return(out);
}'
	
cppFunction(rcpp.append2X)



rcpp.utilde_XtildeFun <- 'NumericMatrix utilde_XtildeFun( NumericMatrix uX, NumericMatrix WRhalf, int k1, int pm1, int n ){
	int nrow = n*pm1*n;
	NumericMatrix out(nrow,k1) ;
	
	
	for(int i1 = 0; i1<n; i1++) {
		for(int k = 0; k<k1; k++){
			for(int i2 = 0; i2<n; i2++)
				for(int j=0; j<pm1; j++)
					out(i1*n*pm1+j+i2*pm1, k) = WRhalf(i1, i2)*uX(j+i2*pm1,k);
		}	
	}
	
	return out;
}'
	
	
cppFunction(rcpp.utilde_XtildeFun)


rcpp.utilde_XtildeFuni <- 'NumericMatrix utilde_XtildeFuni( NumericMatrix uX, NumericVector WRhalfi, int k1, int pm1, int n ){
	int nrow = n*uX.nrow();
	NumericMatrix out(nrow,k1) ;
	
	
	
		for(int k = 0; k<k1; k++){
			for(int i2 = 0; i2<n; i2++)
				for(int j=0; j<uX.nrow(); j++)
					out(j+i2*uX.nrow(), k) = WRhalfi[i2]*uX(j,k);
		}	
	
	
	return out;
}'
	
	
cppFunction(rcpp.utilde_XtildeFuni)



rcpp.setu <- 'NumericVector setu(NumericVector utilde, NumericVector utildei_temp, int pm1, int i_1, int n){
		
	for(int i=0; i<n; i++)
		for(int j =0; j<pm1; j++)
			utilde[n*pm1*i + (i_1-1)*pm1 + j] = utildei_temp[i*pm1 + j];
	
	return(utilde);
}'

cppFunction(rcpp.setu)

