## WE REMOVE mthree FROM THE DATA
## This is to perpare the data

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



########  Calculating the two W matrices

n2 = n
W1=matrix(0, n2,n2)#array(0,dim=c(n2,n2))
lat=as.vector(d1$CUSTLATITUDE)
long=as.vector(d1$CUSTLONGITUDE)
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


W10 = W1
for (i in 1:n2) { W10[i,i]=0}

W11 = W1
iota = array(1,n2)
rsum11=W11%*%iota
W11 = W11/(as.vector(rsum11))

W13=W10
rsum13=W13%*%iota
W13=W13/(as.vector(rsum13))

W15 = W1
for (i in 1:n2) { W15[i,i]=max(W15[i,]) }
rsum15=W15%*%iota
W15=W15/(as.vector(rsum15))

WG = W15


### WG zip code level
WG = matrix(0, n, n)

clusters = d1$ZIP3[d1$MODEL!='']	# Define your group variable here.
tab = table(clusters)

clusters[clusters %in% (names(tab)[tab<50])] = 9999
table(clusters)

WG = matrix(0, n, n)

for(z in unique(clusters))
	WG[which(clusters==z), which(clusters==z)] = 1

for (i in 1:n2) { WG[i,i]=max(WG[i,]) }
rsumG=WG%*%iota
WG=WG/(as.vector(rsumG))

#WG = W15


## Now W mpg data

mpgvars = c('city08', 'highway08', 'comb08', 'UCity', 'UHighway')

mpgcomp1 = as.matrix(d1[d1$MODEL!='',mpgvars]) %*% princomp(d1[d1$MODEL!='',mpgvars])$loadings[,1]

#WV = outer(as.numeric(mpgcomp1), as.numeric(mpgcomp1), function(x, y) 1/(1+abs(x-y)))

WV = outer(as.numeric(mpgcomp1), as.numeric(mpgcomp1), function(x, y) exp(-abs(x-y)))

diag(WV) = apply(WV,1,max)

#for (i in 1:n2) { WV[i,i]=max(WV[i,]) }
rsumV=WV%*%iota
WV=WV/(as.vector(rsumV))


Xdiff <- NULL
for(i in 1:n){
	Xdiff <- rbind(Xdiff,
			X[(i-1)*k+1:(k-1),] - X[(i-1)*k+1:(k-1),k])
}

Sigmadiff <- function(sigma, k){
	S = matrix(sigma[k], k-1, k-1)
	diag(S) <- (sigma[1:(k-1)] + sigma[k])
	S
}

k1 = k; p1 = p
