## This is to calculate the standard errors

target4 <- read.table('target4.txt', sep=' ')
MPG.unique <- read.table('MPG_unique.txt', sep=' ')

m0 = merge(target4, MPG.unique, by=c("TRADENAME", "TRADEMODEL",  "TRADEDISPLACEMENT", "TRADEMY", "TRADEDRIVETYPE" )  )
d <- m0


d$MODEL = factor(as.character(d$MODEL), levels = c("Civic", "Prius", "Corolla", "Sentra", "Civic Hybrid", "xB", "3"))

#attr(d$MODEL, "levels") <- c("Civic", "Prius", "Corolla", "Sentra", "CivicHybrid", "xB", "3")
#c("mthree", "Civic", "CivicHybrid", "Corolla", "Prius", "Sentra", "xB")


table(d$MODEL)
colnames(d)


logprice <- NULL
residualprice <- NULL
pricecrosshybrid <- NULL

price_rebate_promo_100000 <- NULL


hybrids <- c(2, 5)
for(i in 1:7){
	logprice = cbind(logprice, log(d[,paste0('price',i)]))
		

	price_rebate_promo_100000 <- cbind(price_rebate_promo_100000, 
					(d[,paste0('price',i)]-d[,paste0('rebate',i)]-d[,paste0('fpromo',i)])/100000)

	pricecrosshybrid = cbind(pricecrosshybrid, ( (i%in% hybrids)*(d[,paste0('price',i)]-d[,paste0('rebate',i)]-d[,paste0('fpromo',i)]) )/100000 )
}
colnames(logprice) = paste0('logprice', 1:7)
colnames(pricecrosshybrid ) = paste0('pricecrosshybrid', 1:7)
colnames(price_rebate_promo_100000) = paste0('price_rebate_promo_100000_', 1:7)
d <- cbind(d, logprice, pricecrosshybrid, price_rebate_promo_100000)

#priceresiduals = lm(as.numeric(as.matrix(d[,paste0('price',1:7)])) ~ 
#					as.numeric(as.matrix(d[,paste0('dealercost',1:7)])))$residuals

for(i in 1:7){
	residualprice = cbind(residualprice, 
			#priceresiduals[(i-1)*nrow(d) + 1:nrow(d)]/100)
			lm(d[,paste0('price_rebate_promo_100000_',i)]*100000 ~ (d[,paste0('dealercost',i)]) )$residuals/100)
}

colnames(residualprice) = paste0('residualprice', 1:7)
d <- cbind(d, residualprice)

residualpricetimesprice = NULL
for(i in 1:7){
	residualpricetimesprice = cbind(residualpricetimesprice, 
			d[,paste0('residualprice',i)]*d[,paste0('price_rebate_promo_100000_',i)] )
}
colnames(residualpricetimesprice) = paste0('residualpricetimesprice', 1:7)
d <- cbind(d, residualpricetimesprice)



d1 <- d[d$RETAILERDMA == 'SACRAMNTO-STKTON-MODESTO',]
#'SAN FRANCISCO-OAK-SAN JOSE',]#
choice_old = d1[,"MODEL"]

d1 <- d1[d1$mpgData=='Y',]

set.seed(0841)

d1 <- d1[-sample(1:nrow(d1))[1:300],]

choice = d1[,"MODEL"]
choice = as.numeric(choice)

k = max(choice)
n = length(choice)

choice = d1[,"MODEL"]
choice = as.numeric(choice)
choice = choice[d1$MODEL!='']
#choice[choice>1] = choice[choice>1]-1

k = max(choice)
n = length(choice)

X = NULL

#carnames <- c("Civic", "CivicHybrid", "Corolla", "Prius", "Sentra", "xB")
X <- cbind(X, rep(c(1,0,0,0,0,0,0), n)) 
X <- cbind(X, rep(c(0,1,0,0,0,0,0), n))
X <- cbind(X, rep(c(0,0,1,0,0,0,0), n))
X <- cbind(X, rep(c(0,0,0,1,0,0,0), n))
X <- cbind(X, rep(c(0,0,0,0,1,0,0), n))
X <- cbind(X, rep(c(0,0,0,0,0,1,0), n))
X <- cbind(X, rep(c(0,0,0,0,0,0,1), n))



govt_rebate0 = rep(0, nrow(d1))
govt_rebatePrius = rep(787.50 , nrow(d1))
govt_rebatePrius[as.character(d1$MONTHOFSALE) %in% c('Jan 2007', 'Feb 2007', 'Mar 2007')] = 1575
govt_rebatePrius[as.character(d1$MONTHOFSALE) %in% c('Oct 2007', 'Nov 2007', 'Dec 2007')] = 0
govt_rebateCivicHybrid = rep(1700 , nrow(d1))

X <- cbind(X, 
as.numeric(t(cbind(d1$price_rebate_promo_100000_1[d1$MODEL!=''],
d1$price_rebate_promo_100000_2[d1$MODEL!=''],
d1$price_rebate_promo_100000_3[d1$MODEL!=''],
d1$price_rebate_promo_100000_4[d1$MODEL!=''],
d1$price_rebate_promo_100000_5[d1$MODEL!=''],
d1$price_rebate_promo_100000_6[d1$MODEL!=''],
d1$price_rebate_promo_100000_7[d1$MODEL!='']))))


# X <- cbind(X, 
# as.numeric(t(cbind(d1$price1[d1$MODEL!='CivicHybrid'],
# d1$price2[d1$MODEL!='CivicHybrid'],
# #d1$price3[d1$MODEL!='CivicHybrid'],
# d1$price4[d1$MODEL!='CivicHybrid'],
# d1$price5[d1$MODEL!='CivicHybrid'],
# d1$price6[d1$MODEL!='CivicHybrid'],
# d1$price7[d1$MODEL!='CivicHybrid'])))/100000)


# X <- cbind(X, 
# as.numeric(t(cbind((d1$rebate1+d1$fpromo1)[d1$MODEL!='CivicHybrid'],
# (d1$rebate2+d1$fpromo2)[d1$MODEL!='CivicHybrid'],
# #(d1$rebate3+d1$fpromo3),
# (d1$rebate4+d1$fpromo4)[d1$MODEL!='CivicHybrid'],
# (d1$rebate5+d1$fpromo5)[d1$MODEL!='CivicHybrid'],
# (d1$rebate6+d1$fpromo6)[d1$MODEL!='CivicHybrid'],
# (d1$rebate7+d1$fpromo7)[d1$MODEL!='CivicHybrid'])))/100000)


X <- cbind(X, 
as.numeric(t(cbind(d1$residualprice1[d1$MODEL!=''],
d1$residualprice2[d1$MODEL!=''],
d1$residualprice3[d1$MODEL!=''],
d1$residualprice4[d1$MODEL!=''],
d1$residualprice5[d1$MODEL!=''],
d1$residualprice6[d1$MODEL!=''],
d1$residualprice7[d1$MODEL!='']))))

#residualpricetimesprice

X <- cbind(X, 
as.numeric(t(cbind(d1$residualpricetimesprice1[d1$MODEL!=''],
d1$residualpricetimesprice2[d1$MODEL!=''],
d1$residualpricetimesprice3[d1$MODEL!=''],
d1$residualpricetimesprice4[d1$MODEL!=''],
d1$residualpricetimesprice5[d1$MODEL!=''],
d1$residualpricetimesprice6[d1$MODEL!=''],
d1$residualpricetimesprice7[d1$MODEL!='']))))


X <- cbind(X, 
as.numeric(t(cbind(d1$prev.make1[d1$MODEL!=''],
d1$prev.make2[d1$MODEL!=''],
d1$prev.make3[d1$MODEL!=''],
d1$prev.make4[d1$MODEL!=''],
d1$prev.make5[d1$MODEL!=''],
d1$prev.make6[d1$MODEL!=''],
d1$prev.make7[d1$MODEL!='']))))

# c("Civic", "Prius", "Corolla", "Sentra", "Civic Hybrid", "xB", "3")
#X <- cbind(X, 
#as.numeric(t(cbind(govt_rebate0[d1$MODEL!='']/100000,
#		govt_rebatePrius[d1$MODEL!='']/100000,
#		govt_rebate0[d1$MODEL!='']/100000,
#		govt_rebate0[d1$MODEL!='']/100000,
#		govt_rebateCivicHybrid[d1$MODEL!='']/100000,
#		govt_rebate0[d1$MODEL!='']/100000,
#		govt_rebate0[d1$MODEL!='']/100000))))
		
		
p = ncol(X)
y = as.numeric(d1$MODEL)
y = y[d1$MODEL!='']
#y[y>1] = y[y>1]-1



k1 = k; p1 = p

##############
## Load estimates
est <- read.table('Result_latlong_WV_WG_orderfixed_SAC_gaussiankernel.txt', sep=',', row.names=NULL)

#For WGWG # we do not use this.
#est <- read.table('Result_latlong_WG_WG_orderfixed_SAC_gaussiankernel.txt', sep=',', row.names=NULL)

chunk = nrow(d1) + 3

madin = c()
madout = c()

for(itr in 10){	
	ab1 = est[chunk*(itr-1) + (1:(chunk-3)) ,-(1:2)]
	ab1 = as.matrix(ab1)
	S1 =  as.numeric(est[chunk*(itr-1) + ((chunk-3+1)) ,3:9])

}



library(truncnorm)

y0 = runif(n*k, -1, 1)	#initialize the utilities
y1 = y0
ITR = 600
nburn = 50
thin = 8
ab0 = ab1
S0 = S1

ab0 = ab0[rep(1:n, each=k),]
S0 = diag(S0)
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

yGibbs1 = yGibbs[seq(nburn + 1, ITR, thin),]

Xsigma.5 = NULL
for(i in 1:n)
	Xsigma.5 = rbind(Xsigma.5, solve(sqrt(S0))%*%X[(i-1)*k+(1:k),])	

dim(Xsigma.5)
scorefn = NULL
itr = 1
for(itr in 1:nrow(yGibbs1)){

	err = yGibbs1[itr,] - (rowSums(X*ab0))

	scorefn = cbind(scorefn, t(X)%*%(err/rep(diag(S0), n)))
}
vscorefn = var(t(scorefn))

hassianfn = (t(Xsigma.5)%*%Xsigma.5)

estcovmat = solve(hassianfn - vscorefn)
ses = sqrt(diag(estcovmat))
ses
