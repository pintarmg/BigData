##################from benjerry_start.R###################
##########################################################

#### Purchases of Ben and Jerry's Ice Cream
benjer = read.csv("BenAndJerry.csv")

## explore a bit
names(benjer)

#################Question 1 Explore Data##################
##########################################################

head(benjer)
##can see price paid is either deal or non deal, size description is in OZ,
###household size is a number 1-9(+), there are multiple purchases for the
###same houshold

#more people = more ice cream?
boxplot(log(benjer$quantity)~benjer$household_size,xlab="Household Size",
	ylab="Log Quantity")
#difference in price for 16.0 OZ and 32.0 OZ and the deal factor
amntpaid<-(benjer$price_paid_deal+benjer$price_paid_non_deal)/benjer$quantity

deal<-factor(benjer$price_paid_deal>0)
summary(deal)
logpaid<-log(1+amntpaid)
par(mfrow=c(1,2))
boxplot(amntpaid~benjer$size1_descr,ylab="Amount Paid in $",xlab="Product Size")
boxplot(logpaid~deal,ylab="Log Amount Paid",xlab="Promotion")

#purchases from same household
samehouse<-factor(benjer$household_id)
summary(samehouse)
hist(samehouse$count)
samehouse<-table(factor(benjer$household_id))
freq<-as.data.frame(samehouse)
head(freq)
hist(freq$Freq)

#################Question 2 Explain Model#################
##########################################################

#################(cont) benjerry_start.R##################
##########################################################

## create a new variable for price per unit
priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity
y <- log(1+priceper1)

## grab some covariates of interest
## we'll create a properly formatted data.frame
x <- benjer[,c("flavor_descr","size1_descr",
	"household_income","household_size")]

## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr,"VAN")
## coupon usage
x$usecoup = factor(benjer$coupon_value>0)
x$couponper1 <- benjer$coupon_value/benjer$quantity
## organize some demographics
x$region <- factor(benjer$region, 
	levels=1:4, labels=c("East","Central","South","West"))
x$married <- factor(benjer$marital_status==1)
x$race <- factor(benjer$race,
	levels=1:4,labels=c("white","black","asian","other"))
x$hispanic_origin <- benjer$hispanic_origin==1
x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- benjer$type_of_residence==1
x$internet <- benjer$household_internet_connection==1
x$tvcable <- benjer$tv_items>1

## combine x and y, just to follow my recommended `way to use glm'
## cbind is `column bind'.  It takes two dataframes and makes one.
xy <- cbind(x,y)

## fit the regression
fit <- glm(y~., data=xy) 
summary(fit)

###################Improving the Model####################
##########################################################


x2 <- benjer[,c("flavor_descr","size1_descr",
	"household_income","household_size")]
x2$flavor_descr <- relevel(x2$flavor_descr,"VAN")
x2$flavor_descr[x2$flavor_descr=="BROWNIE BATTER"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="BUTTER PECAN"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="CHC"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="CHC ALMOND NOUGAT"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="CHUBBY HUBBY"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="COFFEE"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="DOUBLE CHC FUDGE SWR"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="HEATH CRUNCH"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="MINT CHC CHUNK"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="OATMEAL COOKIE CHUNK"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="PB TRUFFLE"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="RSP CHC CHUNK"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="SMORES"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="STR"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="STRAWBERRIES & CREAM"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="SWEET CREAM & COOKIES"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="VERMONTY PYTHON"]<-"VAN"
x2$flavor_descr[x2$flavor_descr=="WHITE RUSSIAN"]<-"VAN"

x2$flavor_descr[x2$flavor_descr=="DUBLIN MUDSLIDE"]<-"VAN"

x2$usecoup = factor(benjer$coupon_value>0)
x2$couponper1 <- benjer$coupon_value/benjer$quantity
x2$region <- factor(benjer$region, 
	levels=1:4, labels=c("East","Central","South","West"))
x2$married <- factor(benjer$marital_status==1)
x2$race <- factor(benjer$race,
	levels=1:4,labels=c("white","black","asian","other"))
x2$race[x2$race=="asian"]<-"white"
x2$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x2$sfh <- benjer$type_of_residence==1
x2$internet <- benjer$household_internet_connection==1
x2$tvcable <- benjer$tv_items>1
x2$household_id<-factor(benjer$household_id)
                           
x3$household_id<-factor(benjer$household_id)

xy2 <- cbind(x2,y)

## fit the regression
fit2 <- glm(y~., data=xy2) 
summary(fit2)
#############Question 3 P-Value Association###############
##########################################################

## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column
pvals <- summary(fit)$coef[-1,4] 
hist(pvals)

###############Extra Question P-Value FDR#################
##########################################################


## source the fdr_cut function
source("fdr.R")


#######################from fdr.R#########################
##########################################################
###############DO NOT NEED TO DO THIS#####################

## extract p-value cutoff for E[fdf] < q
fdr_cut <- function(pvals, q, plotit=FALSE){
  pvals <- pvals[!is.na(pvals)]
  N <- length(pvals)
  
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals<= (q*k/N) ])
  
  if(plotit){
    sig <- factor(pvals<=alpha)
    o <- order(pvals)
    plot(pvals[o], log="xy", col=c("grey60","red")[sig[o]], pch=20, 
       ylab="p-values", xlab="tests ordered by p-value", main = paste('FDR =',q))
    lines(1:n, q*(1:N)/N)
  }
  
  return(alpha)
}


######################from lipids.R#######################
##########################################################

## find the cut
# @ 10% FDR
cutoff10 <- fdr_cut(pvals,q=.1)
print(cutoff10)
print(sum(pvals<=cutoff10))
# @ 1% FDR
cutoff1 <- fdr_cut(pvals,q=.01)
print(cutoff1)
print(sum(pvals<=cutoff1))
# @ 1/10% FDR
cutoff01 <- fdr_cut(pvals,q=.001)
print(cutoff01)
print(sum(pvals<=cutoff01))



