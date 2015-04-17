##### ******** Mortgage and Home Sales Data ******** #####
#Test
## Read in the data
homes <- read.csv(file.choose())

names(homes)

#bucketize household income
homes$household_income[homes$ZINC2 < 10000] <- 1
homes$household_income[homes$ZINC2 >= 10000 & homes$ZINC2 < 40000] <- 2
homes$household_income[homes$ZINC2 >= 40000 & homes$ZINC2 < 70000] <- 3
homes$household_income[homes$ZINC2 >= 70000 & homes$ZINC2 < 100000] <- 4
homes$household_income[homes$ZINC2 >= 100000 & homes$ZINC2 < 130000] <- 5
homes$household_income[homes$ZINC2 >= 130000 & homes$ZINC2 < 190000] <- 6
homes$household_income[homes$ZINC2 >= 190000 & homes$ZINC2 < 250000] <- 7
homes$household_income[homes$ZINC2 >= 250000] <- 8

par(mfrow=c(1,1)) # 1 row, 2 columns of plots 
# plot relationship of house value vs bucketized household income
plot(VALUE ~ factor(household_income), 
     col=rainbow(8), data=homes,
     xlab="Bucketized Household Income", ylab="home value")

# conditional vs marginal value
par(mfrow=c(1,1)) # 1 row, 2 columns of plots 
hist(homes$VALUE, col="grey", xlab="home value", main="")
plot(VALUE ~ factor(BATHS), 
     col=rainbow(8), data=homes[homes$BATHS<8,],
     xlab="number of bathrooms", ylab="home value")

# plot relationship of house value vs EABAN
plot(VALUE ~ factor(EABAN), 
     col=rainbow(8), data=homes,
     xlab="Abandoned/vandalized bldgs withn 1/2 blk", ylab="home value")

# plot relationship of house value vs HOWN
plot(VALUE ~ factor(HOWN), 
     col=rainbow(8), data=homes,
     xlab="Rating of neighborhood as place to live", ylab="home value")

# plot relationship of EABAN vs HOWN
plot(EABAN ~ factor(HOWN), 
     col=rainbow(8), data=homes,
     xlab="Abandoned/vandalized bldgs withn 1/2 blk", ylab="Rating of neighborhood as place to live")

# plot relationship of VALUE vs FRSTHO
plot(VALUE ~ factor(FRSTHO), 
     col=rainbow(8), data=homes,
     xlab="First Home", ylab="home value")

# plot relationship of VALUE vs DWNPAY
plot(VALUE ~ factor(DWNPAY), 
     col=rainbow(8), data=homes,
     xlab="Main source of downpayment on unit", ylab="home value")

# plot relationship of VALUE vs STATE
plot(VALUE ~ factor(STATE), 
     col=rainbow(8), data=homes,
     xlab="State", ylab="home value")

# plot relationship of VALUE vs EAPTBL
plot(VALUE ~ factor(EAPTBL), 
     col=rainbow(8), data=homes,
     xlab="Apartment bldgs within 1/2 block of unit", ylab="home value")

# plot relationship of VALUE vs HHGRAD
plot(VALUE ~ factor(HHGRAD), 
     col=rainbow(8), data=homes,
     xlab="Educational level of householder", ylab="home value")

# plot relationship of house value vs EABAN
#y <- log(1+homes$ZINC2)
#plot(VALUE ~ factor(ZINC2), 
#     col=rainbow(8), data=homes,
#     xlab="Household Income", ylab="home value")


# create a var for downpayment being greater than 20%
homes$gt20dwn <- 
  factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)

# some quick plots.  Do more to build your intuition!
par(mfrow=c(1,2)) 
plot(VALUE ~ STATE, data=homes, 
     col=rainbow(nlevels(homes$STATE)), 
     ylim=c(0,10^6), cex.axis=.65)
plot(gt20dwn ~ FRSTHO, data=homes, 
     col=c(1,3), xlab="Buyer's First Home?", 
     ylab="Greater than 20% down")

## code hints 

## Q2 
# regress log(VALUE) on everything except AMMORT and LPRICE 
pricey <- glm(log(VALUE) ~ .-AMMORT-LPRICE, data=homes)
# extract pvalues
pvals <- summary(pricey)$coef[-1,4]
# example: those variable insignificant at alpha=0.2
names(pvals)[pvals>.1]
# you'll want to replace .2 with your FDR cutoff
# you can use the `-AMMORT' type syntax to drop variables
pricey2<-glm(log(VALUE)~.-AMMORT-LPRICE-ECOM1-EGREEN-ELOW1-ETRANS
                 -ODORA, data=homes)

n<-nrow(homes)
ssr1<-var(pricey$fitted)*(n-1)
sse1<-var(pricey$resid)*(n-1)
rsq1<-ssr1/(ssr1+sse1)
ssr2<-var(pricey2$fitted)*(n-1)
sse2<-var(pricey2$resid)*(n-1)
rsq2<-ssr2/(ssr2+sse2)
cbind(rsq1,rsq2)

##alternate method for regression
# extract pvalues
pvals <- summary(pricey)$coef[-1,4]
source("fdr.R")
cutoff01<-fdr_cut(pvals,q=0.1)
print(cutoff01)
print(sum(pvals<=cutoff01))
names(pvals)[pvals>cutoff01]
##remove variables from regression
pricey2 <-glm(log(VALUE) ~ .-AMMORT-LPRICE-ECOM1-EGREEN-ELOW1-ETRANS-ODORA-PER-ZADULT, data=homes)
summary(pricey2)
dpricey2 <- deviance(pricey2)

##compare rsquare values
rsq1<-1-dpricey/dnull
rsq1
rsq2<-1-dpricey2/dnull
rsq2


## Q3: 
# - don't forget family="binomial"!
pctdwn <- glm(gt20dwn ~ .-AMMORT-LPRICE, data=homes, family="binomial")
summary(pctdwn)
#itnerpret effects for 1st time home buyers and # of bathrooms

# - use +A*B in forumula to add A interacting with B
pctdwn2 <- glm(gt20dwn ~. +BATHS*FRSTHO-AMMORT-LPRICE, data=homes, 
	family="binomial")
summary(pctdwn2)

##mkeenan q3
dwn <- glm(gt20dwn ~ .-AMMORT-LPRICE,data=homes,family="binomial")
summary(dwn)
b <- coef(dwn)
##effect of first home buyers
b["FRSTHOY"]
exp(b["FRSTHOY"])
##if buyer is a first-time homeowner, odds that buyer has put minimum 20% down are multiplied by 0.6907472

##effect of number of bathrooms
b["BATHS"]
exp(b["BATHS"])
##each additional bathroom multiplies odds that buyer has put minimum 20% down by 1.277033

##add interaction variable for baths and first-time buyers
dwn2 <- glm(gt20dwn ~ .-AMMORT-LPRICE+FRSTHO*BATHS,data=homes,family="binomial")
summary(dwn2)
c <- coef(dwn2)
c["BATHS:FRSTHOY"]
exp(c["BATHS:FRSTHOY"])
##if first-time buyer, each additional bathroom multiplies odds of minimum 20% down by 0.8170691

## Q4
# this is your training sample
gt100 <- which(homes$VALUE>1e5)
pctdwntrn <- glm(gt20dwn ~ .+BATHS*FRSTHO-AMMORT-LPRICE, data=homes[gt100,], 
	family="binomial")
ppctdwn <- predict(pctdwntrn,newdata=homes[-gt100,],type="response")

plot(ppctdwn ~ homes$gt20dwn[-gt100],
	xlab="", ylab=c("predicted probability of 20 % down"), 
	col=c("navy","red"))


# ybar and null deviance
source("deviance.R")
D <- deviance(y=homes$gt20dwn[-gt100], pred=ppctdwn, family="binomial")
ybar <- mean(homes$gt20dwn[-gt100]==TRUE)
D0 <- deviance(y=homes$gt20dwn[-gt100], pred=ybar, family="binomial")
1-D/D0

summary(pctdwntrn)
D0t <- 15210
Dt <- 13620
1-Dt/D0t

##mkeenan q4
# this is your training sample
gt100 <- which(homes$VALUE>1e5)
dwntrain <- glm(gt20dwn ~ . -AMMORT-LPRICE+FRSTHO*BATHS,data=homes[gt100,],family="binomial")
summary(dwntrain)
dtrain <- deviance(dwntrain)
dnulltrain <- 15210
rsqsmpl <- 1-dtrain/dnulltrain
pdwn <- predict(dwntrain, newdata=homes[-gt100,], type="response")
par(mfrow=c(1,1))
plot(pdwn ~ homes$gt20dwn[-gt100],xlab="",ylab="probability of min 20% down",col=c("navy","red"))
# ybar and null deviance
source("deviance.R")
D <- deviance(y=homes$gt20dwn[-gt100],pred=pdwn,family="binomial")
ybar <- mean(homes$gt20dwn[-gt100]==TRUE)
D0 <- deviance(y=homes$gt20dwn[-gt100], pred=ybar, family="binomial")
##compare sample and oos rsquare
rsqoos <- 1-D/D0
rsqsmpl
rsqoos
