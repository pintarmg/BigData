library(textir)
library(maptpx) 
library(wordcloud)
data(congress109)


##Q1
cs <-scale(as.matrix(congress109Counts/rowSums(congress109Counts)))

km5 <- kmeans(cs,5)
km10 <- kmeans(cs,10)
km15 <- kmeans(cs,15)
km20 <- kmeans(cs,20)
km25 <- kmeans(cs,25)

kfit <- lapply(1:200, function(k) kmeans(cs,k))

source("kIC.R")

kb5 <- kIC(km5, "B")
kb10 <- kIC(km10, "B")
kb15 <- kIC(km15, "B")
kb20 <- kIC(km20, "B")
kb25 <- kIC(km25, "B")

kb5
kb10
kb15
kb20
kb25
##looks like 5 is the best

##sanity check series of kmeans to be sure
kfit <- lapply(1:200, function(k) kmeans(cs,k))

kaicc <- sapply(kfit,kIC)
kbic <- sapply(kfit,kIC,"B")

plot(kaicc, xlab="K", ylab="IC", 
     ylim=range(c(kaicc,kbic)), 
     bty="n", type="l", lwd=2)
abline(v=which.min(kaicc))
lines(kbic, col=4, lwd=2)
abline(v=which.min(kbic),col=4)
##BIC is set at 0 and curve is increasing, so 5 makes sense

km5$cluster
1- sum(km5$tot.withinss)/km5$totss

##Q2
x <- as.simple_triplet_matrix(congress109Counts)
tpc <- topics(x,K=5*(1:5),verb=10)
summary(tpc,verb=10)
##10-topic model chosen

v <- c()
for(k in 1:10){
  print(k)
  print(rownames(tpc$theta)[order(tpc$theta[,k], decreasing=TRUE)[1:10]])
}

head(tpc$theta)
for(k in 1:10){
  wordcloud(row.names(tpc$theta), 
            freq=tpc$theta[,k], min.freq=0.004, col="maroon")
  
}

##Q3
##check party alignment by kmeans cluster
sum(km5$cluster==1)
sum(km5$cluster==2)
sum(km5$cluster==3)
sum(km5$cluster==4)
sum(km5$cluster==5)
km5

ideo <- congress109Ideology
ideo$cluster <- km5$cluster
ideo$tag <- paste(ideo$cluster,ideo$party)
R <- rep(0,5)
I <- rep(0,5)
D <- rep(0,5)

for(i in 1:5){
  R[i] <- sum(ideo$tag==paste(i,"R"))
  I[i] <- sum(ideo$tag==paste(i,"I"))
  D[i] <- sum(ideo$tag==paste(i,"D"))
}
partytbl <- cBind(R,I,D)
partytbl

##generate table with ideology and topic omega information
ideo2 <- cBind(ideo,tpc$omega)
##subset by party
rsub <- ideo2[ideo2$party=="R",]
dsub <- ideo2[ideo2$party=="D",]
isub <- ideo2[ideo2$party=="I",]
##create empty vectors for mean
RT <- rep(0,10)
IT <- rep(0,10)
DT <- rep(0,10)
##create empty vectors for median
RTm <- rep(0,10)
ITm <- rep(0,10)
DTm <- rep(0,10)
##create empty vectors for standard deviation
RTsd <- rep(0,10)
ITsd <- rep(0,10)
DTsd <- rep(0,10)
##get omega stats by party and topic
for(i in 1:10){
  RT[i] <- mean(rsub[,i+9])
  RTm[i] <- median(rsub[,i+9])
  RTsd[i] <- sd(rsub[,i+9])
  IT[i] <- mean(isub[,i+9])
  ITm[i] <- median(isub[,i+9])
  ITsd[i] <- sd(isub[,i+9])
  DT[i] <- mean(dsub[,i+9])
  DTm[i] <- median(dsub[,i+9])
  DTsd[i] <- sd(dsub[,i+9])
}
##compare topic statistics by party
tmean <- data.frame(R=RT, I=IT, D=DT)
tmed <- data.frame(R=RTm, I=ITm, D=DTm)
tsd <- data.frame(R=RTsd, I=ITsd, D=DTsd)

tmean
tmed
tsd

##turn party to binary with republicans as 1
ideo$party <- as.numeric(ideo$party=="R")

library(gamlr)
cv.repshr <-cv.gamlr(tpc$omega,ideo$repshare,lambda.min.ratio=1E-4)
cv.party <-cv.gamlr(tpc$omega,ideo$party, family="binomial",lambda.min.ratio=1E-4)

plot(cv.repshr)
plot(cv.party)
plot(cv.repshr$gamlr)
plot(cv.party$gamlr)

x1 <-100*congress109Counts/rowSums(congress109Counts)

cv.repshrx1 <-cv.gamlr(x1,ideo$repshare,lambda.min.ratio=1E-4)
cv.partyx1 <-cv.gamlr(x1,ideo$party, family="binomial",lambda.min.ratio=1E-4)

plot(cv.repshrx1)
plot(cv.partyx1)
plot(cv.repshrx1$gamlr)
plot(cv.partyx1$gamlr)

sum(coef(cv.repshr)!=0)
sum(coef(cv.repshrx1)!=0)
sum(coef(cv.party)!=0)
sum(coef(cv.partyx1)!=0)
##less complex models, neither topic model chooses the maximum amount of topics, even with only 10
max(1-cv.repshr$cvm/cv.repshr$cvm[1])
max(1-cv.repshrx1$cvm/cv.repshrx1$cvm[1])
max(1-cv.party$cvm/cv.party$cvm[1])
max(1-cv.partyx1$cvm/cv.partyx1$cvm[1])
##topics have better OOS R2 compared to straight phrase percentages
