source("./R files/jd3_init.R")
source("./R files/jd3_x11.R")
source("./R files/jd3_fractionalairline.R")

uspetroleum<-read.table("./Data/uspetroleum.txt")

y<-uspetroleum[,2]

m1<-jd3_x11(y, 365.25/7,seas.s1="S3X15")
fsa1<-saDecomposition(m1)
m2<-jd3_x11(y, 52)
fsa2<-saDecomposition(m2)
n<-dim(fsa1)[1]
idx<-(n-53):n
plot(idx, fsa1[idx,4], "l")
lines(idx, fsa2[idx,4], col="red")

a1<-jd3_fractionalAirlineDecomposition(log(y), 365.25/7)
fsa3<-saDecomposition(a1)
lines(idx, exp(fsa3[idx,4]), col="blue")


plot(1:dim(fsa1)[1], fsa1[,1], "l", col="gray")
lines(fsa2[,3], col="red")
lines(fsa1[,3], col="blue")

plot(jd3_henderson(result(m1, "d11bis"), length=105), type="l")
plot(jd3_icratios(result(m1,"b1"), result(m1,"d12"), 53))

