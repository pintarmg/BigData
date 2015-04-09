#Tips N Tricks
#Remove NA from vector
promo <- benjer$promotion_type
promo[is.na(promo)] <- 0
#Count Number of times a value is in a vector
#Convert into table
tpp1 <- table(priceper1)
#subset Table
tpp1[names(tpp1)==0]
