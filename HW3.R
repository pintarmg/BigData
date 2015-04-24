## nhl hockey analysis

## the data is in gamlr.  
## You need to first install this, 
## via install.packages("gamlr")

library(gamlr) # loads Matrix as well
help(hockey) # describes the hockey data and shows an example regression

data(hockey) # load the data

# Combine the covariates all together
x <- cBind(config,team,player) # cBind binds together two sparse matrices

# build 'y': home vs away, binary response
y <- goal$homegoal

nhlreg <- gamlr(x, y, 
                free=1:(ncol(config)+ncol(team)), ## free denotes unpenalized columns
                family="binomial", standardize=FALSE)
par(mfrow=c(1,1))
plot(nhlreg)
## coefficients (grab only the players)
# AICc selection 
Baicc <- coef(nhlreg)[colnames(player),]
##get number of non-zero coefficients
sum(Baicc!=0)
##get positive player contributions, exponential functions, and order
nhlpos <- Baicc[Baicc>0]
nhlposfx <- exp(nhlpos)
##look at the top 25
nhlposfx[order(-nhlposfx)[1:25]]
##get negative player contributions, exponential functions, and order
nhlneg <- Baicc[Baicc<0]
nhlnegfx <- exp(nhlneg)
##look at the bottom 25
nhlnegfx[order(nhlnegfx)[1:25]]
#effects of special configurations
Baiccconfig <- coef(nhlreg)[colnames(config),]
Baiccconfig <- exp(Baiccconfig)
Baiccconfig[1:7]
#team-season effects
Baiccteam <- coef(nhlreg)[colnames(team),]
Baiccteam <- exp(Baiccteam)
Baiccteam[order(Baiccteam)[1:25]]
Baiccteam[order(-Baiccteam)[1:25]]

##Question 2
nhlreg2 <- gamlr(x, y, 
                 free=1:(ncol(config)+ncol(team)), ## free denotes unpenalized columns
                 family="binomial", standardize=TRUE)
par(mfrow=c(1,1))
plot(nhlreg2)
Baicc2 <- coef(nhlreg2)[colnames(player),]
##check number of coefficients
sum(Baicc2!=0)
##check maximum value and effect
Baicc2[which.max(Baicc2)]
exp(Baicc2[which.max(Baicc2)])
##check minimum value and effect
Baicc2[which.min(Baicc2)]
exp(Baicc2[which.min(Baicc2)])


##Question 3
##run cross-validation lasso on data
cv.nhlreg <- cv.gamlr(x, y, 
                      free=1:(ncol(config)+ncol(team)), ## free denotes unpenalized columns
                      family="binomial", standardize=FALSE)
##log lambda values for all criteria
log(nhlreg$lambda[which.min(AICc(nhlreg))])
log(nhlreg$lambda[which.min(AIC(nhlreg))])
log(nhlreg$lambda[which.min(BIC(nhlreg))])
log(cv.nhlreg$lambda.min)
log(cv.nhlreg$lambda.1se)
##plot cv and all criteria
ll <- log(nhlreg$lambda) ## the sequence of lambdas
par(mfrow=c(1,2))
plot(cv.nhlreg)
plot(ll, AIC(nhlreg)/nrow(goal), 
     xlab="log lambda", ylab="IC/n", pch=21, bg="orange")
abline(v=ll[which.min(AIC(nhlreg))], col="orange", lty=3)
abline(v=ll[which.min(BIC(nhlreg))], col="green", lty=3)
abline(v=ll[which.min(AICc(nhlreg))], col="black", lty=3)
points(ll, BIC(nhlreg)/nrow(goal), pch=21, bg="green")
points(ll, AICc(nhlreg)/nrow(goal), pch=21, bg="black")
legend("topleft", bty="n",
       fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))


## plot all criteria and cv together
par(mfrow=c(1,1))
plot(nhlreg, col="grey")
abline(v=ll[which.min(AICc(nhlreg))], col="black", lty=2)
abline(v=ll[which.min(AIC(nhlreg))], col="orange", lty=2)
abline(v=ll[which.min(BIC(nhlreg))], col="green", lty=2)
abline(v=log(cv.nhlreg$lambda.min), col="blue", lty=2)
abline(v=log(cv.nhlreg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, 
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV.min","CV.1se"))

##Question 4
##run regression with only player effects
nhlreg3 <- gamlr(player, y, family="binomial", standardize=FALSE, lambda.min.ratio=0.00001)
par(mfrow=c(1,1))
plot(nhlreg3)
## coefficients (grab only the players)
# AICc selection 
Baicc3 <- coef(nhlreg3)[colnames(player),]
##get number of non-zero coefficients
sum(Baicc3!=0) ##all of them, makes sense if there are no other effects
cv.nhlreg3 <- cv.gamlr(player, y, family="binomial", standardize=FALSE, lambda.min.ratio=0.00001)
plot(cv.nhlreg3) ##roughly the same as AICc
cv.min.Baicc3 <- coef(cv.nhlreg3,select="min")[colnames(player),]
cv.1se.Baicc3 <- coef(cv.nhlreg3,select="1se")[colnames(player),]
sum(cv.min.Baicc3!=0)
sum(cv.1se.Baicc3!=0)
##Question +
##get unique seasons and create tables for plus-minus and predicted plus-minus
sn <- unique(goal$season)
tbl.pm <- data.frame(season=sn,player=rep(NA,11),ppm=rep(NA,11),pm=rep(NA,11))
tbl.ppm <- data.frame(season=sn,player=rep(NA,11),ppm=rep(NA,11),pm=rep(NA,11))

##loop for each season to get the predicted plus-minus and plus-minus leaders
for(k in 1:11){
  
  now <- goal$season==sn[k]
  pm <- colSums(player[now,names(Baicc)]) # traditional plus minus
  ng <- colSums(abs(player[now,names(Baicc)])) # total number of goals
  # The individual effect on probability that a given goal is for vs against that player's team
  p <- 1/(1+exp(-Baicc))
  # multiply ng*p - ng*(1-p) to get expected plus-minus
  ppm <- ng*(2*p-1)
  ## Organize data
  effect <- data.frame(b=round(Baicc,3),ppm=round(ppm,3),pm=pm)
  ##sort by plus-minus
  effect1 <- effect[order(-effect$pm),]
  ##extract names
  name1 <- attr(effect1,"row.names")
  ##sort by predicted plus-minus
  effect2 <- effect[order(-effect$ppm),]
  ##extract names
  name2 <- attr(effect2,"row.names")
  ##add leaders to existing tables
  tbl.pm$player[k] <- name1[1]
  tbl.pm$pm[k] <- effect$pm[order(-effect$pm)[1]]
  tbl.pm$ppm[k] <- effect$ppm[order(-effect$pm)[1]]
  tbl.ppm$player[k] <- name2[1]
  tbl.ppm$pm[k] <- effect$pm[order(-effect$ppm)[1]]
  tbl.ppm$ppm[k] <- effect$ppm[order(-effect$ppm)[1]]
  ##erase objects
  now <- NULL
  pm <- NULL
  ng <- NULL
  p <- NULL
  ppm <- NULL
  effect <- NULL
  effect1 <- NULL
  effect2 <- NULL
  name1<- NULL
  name2 <- NULL 
}

##view results for each season
tbl.pm ##plus-minus leaders all goalies except for Niklas Backstrom
tbl.ppm ## predicted plus-minus leaders all forwards, with actuals far below predicted values
##########################################################
##original plus-minus calculation and application
##traditional career plus-minus
## convert to 2013-2014 season partial plus-minus
now <- goal$season=="20132014"
pm <- colSums(player[now,names(Baicc)]) # traditional plus minus
ng <- colSums(abs(player[now,names(Baicc)])) # total number of goals
# The individual effect on probability that a
# given goal is for vs against that player's team
p <- 1/(1+exp(-Baicc))
# multiply ng*p - ng*(1-p) to get expected plus-minus
ppm <- ng*(2*p-1)

# organize the data together and print top 20
effect <- data.frame(b=round(Baicc,3),ppm=round(ppm,3),pm=pm)
effect <- effect[order(-effect$ppm),]
print(effect[1:20,])
