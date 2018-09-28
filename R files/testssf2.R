source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")
source("./R files/jd3_utility.R")


load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-ABS$X0.2.09.10.M

# create the model
bsm<-jd3_ssf_model()

# create the components and add them to the model
add(bsm, jd3_ssf_locallineartrend("ll"))
add(bsm, jd3_ssf_seasonal("s", 12, type="Crude"))
add(bsm, jd3_ssf_td("td", frequency(s), start(s), length(s), c(1,1,1,1,1,0,0), contrast = FALSE, variance = .0001, fixed=FALSE))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", 1, TRUE)
add(eq, "ll")
add(eq, "s")
add(eq, "td")
add(bsm, eq)
#estimate the model
rslt<-estimate(bsm, log(s), 1e-20, marginal=FALSE, concentrated=TRUE)
print(result(rslt, "parameters"))
print(result(rslt, "loglikelihood"))
print(result(rslt, "scalingfactor"))

plot(result(rslt, "ssf.smoothing.array(13)"), type="l", ylim=c(-.01, .02))
#lines(result(rslt, "ssf.smoothing.array(14)"), col="red" )
#lines(result(rslt, "ssf.smoothing.array(16)"), col="blue" )

# Raw
# create the model
rbsm<-jd3_ssf_model()
# create the components and add them to the model
add(rbsm, jd3_ssf_locallineartrend("ll"))
add(rbsm, jd3_ssf_seasonal("s", 12, type="Crude"))
add(rbsm, jd3_ssf_rawtd("td", frequency(s), start(s), length(s), c(1,1,1,1,1,2,0), c(.001, .001, .001), fixed=FALSE))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", 1, TRUE)
add(eq, "ll")
add(eq, "s")
add(eq, "td")
add(rbsm, eq)
eqs<-jd3_ssf_equation("eqs", 0, TRUE)
add(eqs, "td", loading=jd3_ssf_loading_sum())
add(rbsm, eqs)

#estimate the model
all<-cbind(log(s), 0)
rrslt<-estimate(rbsm, all, 1e-20, marginal=FALSE, concentrated=TRUE)
print(result(rrslt, "parameters"))
print(result(rrslt, "loglikelihood"))
print(result(rrslt, "scalingfactor"))

d1<-result(rrslt, "ssf.smoothing.array(13)")
d2<-result(rrslt, "ssf.smoothing.array(14)")
m<-min(d1,d2)
M<-max(d1,d2)
plot(d1, type="l", ylim=c(m, M))
lines(d2, col="red" )


# create the model
bsm2<-jd3_ssf_model()

# create the components and add them to the model
add(bsm2, jd3_ssf_locallineartrend("ll", levelVariance = 1, fixedLevelVariance = TRUE, slopevariance = .01, fixedSlopeVariance = FALSE))
add(bsm2, jd3_ssf_seasonal("s", 12, type="Crude", 1, TRUE))
add(bsm2, jd3_ssf_noise("n", 1, TRUE))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", 0, TRUE)
add(eq, "ll", 1, FALSE)
add(eq, "s", 1, FALSE)
add(eq, "n", 1, FALSE)
add(bsm2, eq)
#estimate the model
rslt2<-estimate(bsm2, s, 1e-20, marginal=TRUE, concentrated=FALSE)
print(result(rslt2, "parameters"))
print(result(rslt2, "loglikelihood"))
print(result(rslt2, "scalingfactor"))

# create the model
bsm3<-jd3_ssf_model()

# create the components and add them to the model
add(bsm3, jd3_ssf_noise("n", 1, TRUE))
add(bsm3, jd3_ssf_locallineartrend("ll"))
add(bsm3, jd3_ssf_seasonal("s", 12, type="Crude"))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", 0, TRUE)
add(eq, "ll")
add(eq, "n")
add(eq, "s")
add(bsm3, eq)
#estimate the model
rslt<-estimate(bsm3, s, marginal=TRUE, concentrated=TRUE)
print(result(rslt, "parameters"))
print(result(rslt, "loglikelihood"))
print(result(rslt, "scalingfactor"))

