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

plot(1:dim(fsa1)[1], fsa1[,1], "l", col="gray")
lines(fsa2[,3], col="red")
lines(fsa1[,3], col="blue")

m1<-jd3_fractionalAirlineDecomposition(log(usclaims[,2]), 365.25/7)
show(m1)
fsa1<-saDecomposition(m1)
m2<-jd3_fractionalAirlineDecomposition(log(usclaims[,2]), 365.25/7, FALSE)
show(m2)
fsa2<-saDecomposition(m2)
n<-dim(fsa1)[1]
idx<-(n-206):n
plot(idx, fsa1[idx,4], "l")
lines(idx, fsa2[idx,4], col="red")

plot(1:dim(fsa1)[1], fsa1[,1], "l", col="gray")
lines(fsa2[,3], col="red")
lines(fsa1[,3], col="blue")

test_ll<-function(series, low, high, step){
  p<-seq(low, high, by=step)
  ll<-rep(NA, length(p))
  i<-1
  for (q in p){
    m<-jd3_fractionalAirlineDecomposition(series, q, TRUE)
    cat(q)
    cat('\t')
    cat(proc_numeric(m@internal, "likelihood.ll"))
    cat('\n')    
    if (! is.null(m@internal)){
    ll[i]<-proc_numeric(m@internal, "likelihood.ll")
    }
    i<-i+1
  }
  return (ll)
}