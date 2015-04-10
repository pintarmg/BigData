##### ******** Mortgage and Home Sales Data ******** #####

## Read in the data
homes <- read.csv(file.choose())

names(homes)

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

fit <- glm(VALUE ~ ., data=homes)
summary(fit)
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
names(pvals)[pvals>.2]
# you'll want to replace .2 with your FDR cutoff
# you can use the `-AMMORT' type syntax to drop variables

## Q3: 
# - don't forget family="binomial"!
# - use +A*B in forumula to add A interacting with B

## Q4
# this is your training sample
gt100 <- which(homes$VALUE>1e5)
# ybar and null deviance
source("deviance.R")

ybar <- mean(homes$gt20dwn[-gt100]==TRUE)
D0 <- deviance(y=homes$gt20dwn[-gt100], pred=ybar, family="binomial")




