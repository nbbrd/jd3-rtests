source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")
source("./R files/jd3_utility.R")


load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-log(ABS$X0.2.20.10.M)

# create the model
bsm<-jd3_ssf_model()

# create the components and add them to the model
ssf.add(bsm, jd3_ssf_locallineartrend("ll"))
ssf.add(bsm, jd3_ssf_seasonal("s", 12, type="Crude"))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", 1, TRUE)
ssf.add(eq, "ll", 1, FALSE)
ssf.add(eq, "s", 1, FALSE)
ssf.add(bsm, eq)
#estimate the model
rslt<-ssf.estimate(bsm, s, marginal=TRUE, concentrated=TRUE)
print(result(rslt, "parameters"))
print(result(rslt, "loglikelihood"))
print(result(rslt, "scalingfactor"))


# create the model
bsm2<-jd3_ssf_model()

# create the components and add them to the model
ssf.add(bsm2, jd3_ssf_locallineartrend("ll", levelVariance = 1, fixedLevelVariance = TRUE, slopevariance = .01, fixedSlopeVariance = FALSE))
ssf.add(bsm2, jd3_ssf_seasonal("s", 12, type="Crude", 1, TRUE))
ssf.add(bsm2, jd3_ssf_noise("n", 1, TRUE))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", 0, TRUE)
ssf.add(eq, "ll", 1, FALSE)
ssf.add(eq, "s", 1, FALSE)
ssf.add(eq, "n", 1, FALSE)
ssf.add(bsm2, eq)
#estimate the model
rslt<-ssf.estimate(bsm2, s, marginal=TRUE, concentrated=FALSE)
print(result(rslt, "parameters"))
print(result(rslt, "loglikelihood"))
print(result(rslt, "scalingfactor"))

# create the model
bsm3<-jd3_ssf_model()

# create the components and add them to the model
ssf.add(bsm3, jd3_ssf_noise("n", 1, TRUE))
ssf.add(bsm3, jd3_ssf_locallineartrend("ll"))
ssf.add(bsm3, jd3_ssf_seasonal("s", 12, type="Crude"))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", 0, TRUE)
ssf.add(eq, "ll")
ssf.add(eq, "n")
ssf.add(eq, "s")
ssf.add(bsm3, eq)
#estimate the model
rslt<-ssf.estimate(bsm3, s, marginal=TRUE, concentrated=TRUE)
print(result(rslt, "parameters"))
print(result(rslt, "loglikelihood"))
print(result(rslt, "scalingfactor"))

# create the model
bsm4<-jd3_ssf_model()

# create the components and add them to the model
ssf.add(bsm4, jd3_ssf_locallineartrend("ll"))
ssf.add(bsm4, jd3_ssf_seasonal("s", 12, type="Crude"))
ssf.add(bsm4, jd3_ssf_td("td", frequency(s), start(s), length(s), c(1,2,3,4,5,6,0), TRUE, 0, FALSE))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq",1, TRUE)
ssf.add(eq, "ll")
ssf.add(eq, "s")
ssf.add(eq, "td")
ssf.add(bsm4, eq)
#estimate the model
rslt<-ssf.estimate(bsm4, s, marginal=TRUE, concentrated=TRUE)
print(result(rslt, "parameters"))
print(result(rslt, "loglikelihood"))
print(result(rslt, "scalingfactor"))
