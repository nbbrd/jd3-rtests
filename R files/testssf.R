source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")
source("./R files/jd3_utility.R")


load("./Data/retail.rda")
load("./Data/ABS.rda")

# Example, arima with TD
s<-log(ABS$X0.2.09.10.M)

ssf1<-jd3_ssf_sarima(12, c(0,1,1), c(0,1,1), c(-.6, -.8))

print(jd3_ssf_dk_concentratedlikelihood(ssf1, s))

X<-jd3_calendar(c(1,2,3,4,5,6,0), TRUE, frequency(s), start(s)[1], start(s)[2],length(s))

ssf2<-jd3_ssf_reg(ssf1, X)

print(jd3_ssf_dk_concentratedlikelihood(ssf2, s))

ssf3<-jd3_ssf_reg(ssf1, X, var=0.01)

print(jd3_ssf_dk_concentratedlikelihood(ssf3, s))

fn1<-function(x, s){
  ls<-log(s)
  ssf<-jd3_ssf_sarima(12, c(0,1,1), c(0,1,1), x)
  return(-jd3_ssf_dk_concentratedlikelihood(ssf, ls))
}

fn2<-function(x, s, X){
  ls<-log(s)
  ssf<-jd3_ssf_sarima(12, c(0,1,1), c(0,1,1), x)
  ssf2<-jd3_ssf_reg(ssf, X)
  return(-jd3_ssf_dk_concentratedlikelihood(ssf2, ls))
}

fn3<-function(x, s, X){
  ls<-log(s)
  z<-c(x[1], x[2])
  ssf<-jd3_ssf_sarima(12, c(0,1,1), c(0,1,1), z)
  ssf2<-jd3_ssf_reg(ssf, X, x[3])
  return(-jd3_ssf_dk_concentratedlikelihood(ssf2, ls))
}

fn4<-function(x, s, X){
  ls<-log(s)
  z<-c(x[1], x[2], x[3], x[4], x[5])
  ssf<-jd3_ssf_sarima(12, c(3,1,1), c(0,1,1), z)
  ssf2<-jd3_ssf_reg(ssf, X, x[6])
  return(-jd3_ssf_dk_concentratedlikelihood(ssf2, ls))
}

print(optim(c(-.6, -.8), fn1, NULL, ABS$X0.2.20.10.M, method = "BFGS", hessian=TRUE))
print(optim(c(-.6, -.8), fn2, NULL, ABS$X0.2.20.10.M, X, method = "BFGS", hessian=TRUE))
print(optim(c(-.6, -.8, 0.01), fn3, NULL, ABS$X0.2.20.10.M, X, method = "BFGS", hessian=TRUE))
print(optim(c(-.2, -.2, -.2, -.6, -.8, 0.01), fn4, NULL, ABS$X0.2.20.10.M, X, method = "BFGS", hessian=TRUE))
print(optim(c(-.6, -.8), fn1, NULL, ABS$X0.2.09.10.M, method = "BFGS", hessian=TRUE))
print(optim(c(-.6, -.8), fn2, NULL, ABS$X0.2.09.10.M, X, method = "BFGS", hessian=TRUE))
print(optim(c(-.6, -.8, 0.01), fn3, NULL, ABS$X0.2.09.10.M, X, method = "BFGS", hessian=TRUE))
print(optim(c(-.2, -.2, -.2, -.6, -.8, 0.01), fn4, NULL, ABS$X0.2.09.10.M, X, method = "BFGS", hessian=TRUE))
