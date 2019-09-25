 source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")

load("./Data/ABS.rda")

s<-ABS$X0.2.20.10.M

# create the model
bsm1<-jd3_ssf_model()
# create the components and add them to the model
ssf.add(bsm1, jd3_ssf_locallineartrend("ll"))
ssf.add(bsm1, jd3_ssf_seasonal("seas", 12, type="Trigonometric"))
ssf.add(bsm1, jd3_ssf_td("td", frequency(s), start(s), length(s), c(1,1,1,1,2,3,0)))
# create the equation 
eq1<-jd3_ssf_equation("eq", 1, T)
ssf.add(eq1, "ll")
ssf.add(eq1, "seas")
ssf.add(eq1, "td")
ssf.add(bsm1, eq1)
#estimate the model
rslt1<-ssf.estimate(bsm1, log(s), marginal=TRUE, concentrated=TRUE)
print(result(rslt1, "loglikelihood"))
print(result(rslt1, "scalingfactor"))

print(result(rslt1, "parameternames"))
print(result(rslt1, "parameters"))

s1<-(result(rslt1, "ssf.smoothing.states"))
f1<-(result(rslt1, "ssf.filtering.states"))

plot(as.double(log(s)), type='l')
lines(f1[,1], col="blue")
lines(s1[,1], col="red")

# create the model
bsm2<-jd3_ssf_model()
# create the components and add them to the model
ssf.add(bsm2, jd3_ssf_locallineartrend("ll"))
ssf.add(bsm2, jd3_ssf_seasonal("seas", 12, type="Trigonometric", variance = 1, fixed = TRUE))
ssf.add(bsm2, jd3_ssf_td("td", frequency(s), start(s), length(s), c(1,1,1,1,2,3,0)))
# create the equation 
eq2<-jd3_ssf_equation("eq", 1, T)
ssf.add(eq2, "ll")
ssf.add(eq2,"seas", coef=.1, fixed=F)
ssf.add(eq2, "td")
ssf.add(bsm2, eq2)
#estimate the model
rslt2<-ssf.estimate(bsm2, log(s), marginal=TRUE, concentrated=TRUE)
print(result(rslt2, "loglikelihood"))
print(result(rslt2, "scalingfactor"))

print(result(rslt2, "parameternames"))
print(result(rslt2, "parameters"))

s2<-(result(rslt2, "ssf.smoothing.states"))
f2<-(result(rslt2, "ssf.filtering.states"))

plot(as.double(log(s)), type='l')
lines(f2[,1], col="blue")
lines(s2[,1], col="red")

# create the model
bsm3<-jd3_ssf_model()
# create the components and add them to the model
ssf.add(bsm3, jd3_ssf_locallevel("l", initial = 0))
ssf.add(bsm3, jd3_ssf_locallineartrend("t", levelVariance = 0, fixedLevelVariance = T))
ssf.add(bsm3, jd3_ssf_seasonal("seas", 12, type="Trigonometric"))
ssf.add(bsm3, jd3_ssf_td("td", frequency(s), start(s), length(s), c(1,1,1,1,2,3,0)))
ssf.add(bsm3, jd3_ssf_noise("n", variance = 1, fixed = T))
# create the equation (fix the variance to 1)
eq3<-jd3_ssf_equation("eq", 0, T)
ssf.add(eq3, "l")
ssf.add(eq3, "t")
ssf.add(eq3, "seas")
ssf.add(eq3, "td")
ssf.add(eq3, "n")
ssf.add(bsm3, eq3)
#estimate the model
rslt3<-ssf.estimate(bsm3, log(s), marginal=TRUE, concentrated=TRUE)
print(result(rslt3, "loglikelihood"))
print(result(rslt3, "scalingfactor"))

print(result(rslt3, "parameternames"))
print(result(rslt3, "parameters"))

s3<-(result(rslt3, "ssf.smoothing.states"))
f3<-(result(rslt3, "ssf.filtering.states"))

plot(as.double(log(s)), type='l')
lines(f3[,1]+f3[,2], col="blue")
lines(s3[,1]+s3[,2], col="red")