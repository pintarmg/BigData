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

library(gamlr)
cv.repshr <-cv.gamlr(tpc$omega,ideo$repshare,lambda.min.ratio=1E-4)
cv.party <-cv.gamlr(tpc$omega,ideo$party,lambda.min.ratio=1E-4)

plot(cv.repshr)
plot(cv.party)
plot(cv.repshr$gamlr)
plot(cv.party$gamlr)

x1 <-100*congress109Counts/rowSums(congress109Counts)

cv.repshrx1 <-cv.gamlr(x1,ideo$repshare,lambda.min.ratio=1E-4)
cv.partyx1 <-cv.gamlr(x1,ideo$party,lambda.min.ratio=1E-4)

plot(cv.repshrx1)
plot(cv.partyx1)
plot(cv.repshrx1$gamlr)
plot(cv.partyx1$gamlr)

max(1-cv.repshr$cvm/cv.repshr$cvm[1])
max(1-cv.repshrx1$cvm/cv.repshrx1$cvm[1])
max(1-cv.party$cvm/cv.party$cvm[1])
max(1-cv.partyx1$cvm/cv.partyx1$cvm[1])
##topics have MUCH better OOS R2 compared to straight phrase percentages
