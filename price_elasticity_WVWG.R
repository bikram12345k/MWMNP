## Price elasticity calculations


source('RcppSupportv2_new.R')

##############
## Load estimates
est <- read.table('Result_latlong_WV_WG_orderfixed_SAC_gaussiankernel.txt', sep=',', row.names=NULL)

#For WGWG # we do not use this.
#est <- read.table('Result_latlong_WG_WG_orderfixed_SAC_gaussiankernel.txt', sep=',', row.names=NULL)


chunk = 1896 + 3

# find top 
itr = 30

ab1 = est[chunk*(itr-1) + (1:(chunk-3)) ,-(1:2)]
ab1 = as.matrix(ab1)
S1 =  as.numeric(est[chunk*(itr-1) + ((chunk-3+1)) ,3:9])

Sigmadiff <- function(sigma, k){
	S = matrix(sigma[k], k-1, k-1)
	diag(S) <- (sigma[1:(k-1)] + sigma[k])
	S
}
##################



target4 <- read.table('target4.txt', sep=' ')
MPG.unique <- read.table('MPG_unique.txt', sep=' ')

m0 = merge(target4, MPG.unique, by=c("TRADENAME", "TRADEMODEL",  "TRADEDISPLACEMENT", "TRADEMY", "TRADEDRIVETYPE" )  )


## repeat this process for discount on all prices for all cars

PjsDisct <- list()

for(disctCar in 1:7){

d <- m0

d$MODEL = factor(as.character(d$MODEL), levels = c("Civic", "Prius", "Corolla", "Sentra", "Civic Hybrid", "xB", "3"))

logprice <- NULL
residualprice <- NULL
pricecrosshybrid <- NULL

price_rebate_promo_100000 <- NULL

d[,paste0('price',disctCar)] = d[,paste0('price',disctCar)]*.99


hybrids <- c(2, 5)
for(i in 1:7){
	logprice = cbind(logprice, log(d[,paste0('price',i)]))
		

	price_rebate_promo_100000 <- cbind(price_rebate_promo_100000, 
					(d[,paste0('price',i)]-d[,paste0('rebate',i)]-d[,paste0('fpromo',i)])/100000)

	pricecrosshybrid = cbind(pricecrosshybrid, ( (i%in% hybrids)*(d[,paste0('price',i)]-d[,paste0('rebate',i)]-d[,paste0('fpromo',i)]) )/100000 )
}
colnames(logprice) = paste0('logprice', 1:7)
colnames(pricecrosshybrid ) = paste0('pricecrosshybrid', 1:7)
colnames(price_rebate_promo_100000) = paste0('price_rebate_promo_100000_old_', 1:7)
d <- cbind(d, logprice, pricecrosshybrid, price_rebate_promo_100000)



price_rebate_promo_100000 <- NULL

for(i in 1:7){
	price_rebate_promo_100000 <- cbind(price_rebate_promo_100000, 
					(d[,paste0('price',i)]-d[,paste0('rebate',i)]-d[,paste0('fpromo',i)])/100000)
}

colnames(price_rebate_promo_100000) = paste0('price_rebate_promo_100000_', 1:7)
d <- cbind(d, price_rebate_promo_100000)




#priceresiduals = lm(as.numeric(as.matrix(d[,paste0('price',1:7)])) ~ 
#					as.numeric(as.matrix(d[,paste0('dealercost',1:7)])))$residuals

for(i in 1:7){
	residualprice = cbind(residualprice, 
			#priceresiduals[(i-1)*nrow(d) + 1:nrow(d)]/100)
			lm(d[,paste0('price_rebate_promo_100000_old_',i)]*100000 ~ (d[,paste0('dealercost',i)]) )$residuals/100)
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


d$id = 1:nrow(d)

d1 <- d[d$RETAILERDMA == 'SACRAMNTO-STKTON-MODESTO',]
choice_old = d1[,"MODEL"]

d1 <- d1[d1$mpgData=='Y',]

set.seed(0841)
d3 <- d1 

Outunits <- sample(1:nrow(d1))[1:300] 
d1 <- d1[Outunits,]
d4 = d1

rm(d1)
load('validation_data')

d1 = d[(d$id %in% d1$id),]

choice = d1[,"MODEL"]
table(choice)
choice = as.numeric(choice)

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



Xdiff <- NULL
for(i in 1:n){
	Xdiff <- rbind(Xdiff,
			X[(i-1)*k+(1:(k-1)),] - X[(i-1)*k+k,,drop=FALSE][rep(1,(k-1)),])
}



k1 = k; p1 = p




#############
## Need the extended W matrices

### WG zip code level
clusters = d3$ZIP3	# Define your group variable here.
tab = table(d1$ZIP3)

clusters[clusters %in% (names(tab)[tab<50])] = 9999
table(clusters)

WG = matrix(0, nrow(d3), nrow(d3))

for(z in unique(clusters))
	WG[which(clusters==z), which(clusters==z)] = 1

iota = array(1,nrow(d3))

for (i in 1:nrow(d3)) { WG[i,i]=max(WG[i,]) }
rsumG=WG%*%iota
#WG=WG/(as.vector(rsumG))



n2 = nrow(d3)
W1=matrix(0, n2,n2)#array(0,dim=c(n2,n2))
lat=as.vector(d3$CUSTLATITUDE)
long=as.vector(d3$CUSTLONGITUDE)
dlat=matrix(0, n2,n2)#array(0, dim=c(n2,n2))
dlong=matrix(0, n2,n2)#array(0, dim=c(n2,n2))
temp.a=matrix(0, n2,n2)#array(0, dim=c(n2,n2))
temp.c=matrix(0, n2,n2)#array(0, dim=c(n2,n2))
for (i in 1:n2){
  for (j in 1:n2){
    dlat[i,j] = lat[i] - lat[j]
    dlong[i,j] = long[i] - long[j]
    
    # sqrt( (69.1*dlat[i,j])^2 + (53*dlong[i,j])^2 )  -> distance in miles
    # sqrt( (69.1*dlat[i,j])^2 + (53*dlong[i,j])^2 ) * 1.609344  -> distance in kilometers
    W1[i,j] = 1/exp(sqrt( (69.1*dlat[i,j])^2 + (53*dlong[i,j])^2 ) )  
    
    if (dlat[i,j]==0 & dlong[i,j]==0) {W1[i,j]=0}
  }
  W1[j,i] = W1[i,j]
  W1[i,i] = 0
}

W1=as.matrix(W1)
iota = array(1,n2)

W15 = W1
for (i in 1:n2) { W15[i,i]=max(W15[i,]) }
rsum15=W15%*%iota
#W15=W15/(as.vector(rsum15))

WG = W15


## Now W mpg data

mpgvars = c('city08', 'highway08', 'comb08', 'UCity', 'UHighway')

mpgcomp1 = as.matrix(d3[,mpgvars]) %*% princomp(d1[,mpgvars])$loadings[,1]

WV = outer(as.numeric(mpgcomp1), as.numeric(mpgcomp1), function(x, y) exp(-abs(x-y))) #1/(1+abs(x-y)))

for (i in 1:nrow(d3)) { WV[i,i]=max(WV[i,]) }
rsumV=WV%*%iota
#WV=WV/(as.vector(rsumV))

## 
WGout = WG[which(d3$id %in% d1$id), -which(d3$id %in% d1$id)]
WVout = WV[which(d3$id %in% d1$id), -which(d3$id %in% d1$id)]
dim(WGout)

WGout = t(apply(WGout, 1, function(x) x/sum(x)))
WVout = t(apply(WVout, 1, function(x) x/sum(x)))


##############

Wpreference = WVout
Wresponse = WGout


about = cbind(Wpreference%*%ab1[,1:k], Wresponse%*%ab1[,(k+1):p])

#mean( deprintize(llmnp3)(t(about ), Sigmadiff(S1, k), Xdiff , y, r=300)$Pj )

Pjs = NULL
for(j in 1:7){
	Pjs = cbind(Pjs, deprintize(llmnp3)(t(about ), Sigmadiff(S1, k), Xdiff , rep(j, nrow(d1)), r=1000)$Pj)
}
#Pjs = deprintize(llmnp3)(t(ab1 ), Sigmadiff(S1, k), Xdiff , y, r=300)$Pj


PjsDisct[[disctCar]] = Pjs

}

#Pjs0 = Pjs

sapply(1:7, function(disctCar) (colSums(PjsDisct[[disctCar]])-colSums(Pjs0))/colSums(Pjs0)*100)
