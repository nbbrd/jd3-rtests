source("./R files/jd3_init.R")
source("./R files/jd3_fractionalairline.R")

usclaims<-read.table("./Data/usclaims.txt")

m1<-jd3_fractionalAirlineDecomposition(log(usclaims[,1]), 365.25/7)
show(m1)
fsa1<-saDecomposition(m1)
m2<-jd3_fractionalAirlineDecomposition(log(usclaims[,1]), 365.25/7, FALSE)
show(m2)
fsa2<-saDecomposition(m2)
n<-dim(fsa1)[1]
idx<-(n-206):n
plot(idx, fsa1[idx,4], "l")
lines(idx, fsa2[idx,4], col="red")

plot(1:dim(fsa1)[1], fsa1[,3], "l")
lines(fsa2[,3], col="red")
