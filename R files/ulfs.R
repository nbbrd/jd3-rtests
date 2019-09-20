source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")

load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-ABS$X0.2.09.10.M
# create the model
bsm<-jd3_ssf_model()

# create the components and add them to the model
# trend component
ssf.add(bsm, jd3_ssf_locallineartrend("ll"))
# or, for a smooth trend: 
# add(bsm, jd3_ssf_locallineartrend("ll", levelVariance = 0, fixedLevelVariance = TRUE))

# seasonal component. Several specifcations available
ssf.add(bsm, jd3_ssf_seasonal("s", 12, type="Dummy"))
# for instance: add(bsm, jd3_ssf_seasonal("s", 12, type="Trigonometric"))
# or my preferred one (trivial extensions to seasonal specific components)
# add(bsm, jd3_ssf_seasonal("s", 12, type="HarrisonStevens"))

#sample error
ssf.add(bsm, jd3_ssf_sae("n", c(.3), lag=3))
# AR(3,6) with fixed parameters
# add(bsm, jd3_ssf_sae("n", c(.3, -2), fixedar=TRUE, lag=3))

# Survey std error
f<-10+runif(n=length(s), min = 0, max=10)

# create the equation (fix the variance to 0)
eq<-jd3_ssf_equation("eq", 0, TRUE)
ssf.add(eq, "ll")
ssf.add(eq, "s")
ssf.add(eq, "n", loading= jd3_ssf_varloading(0, f))
ssf.add(bsm, eq)

rslt<-ssf.estimate(bsm, s, concentrated=FALSE)
print(result(rslt, "loglikelihood"))
print(result(rslt, "parameters"))

print(result(rslt, "ssf.cmppos"))
plot(result(rslt, "ssf.smoothing.cmp(0)"), type="l")
lines(result(rslt, "ssf.filtering.cmp(0)"), col="red")

plot(result(rslt, "ssf.smoothing.cmp(1)"), type="l")
plot(result(rslt, "ssf.smoothing.cmp(2)"), type="l")

