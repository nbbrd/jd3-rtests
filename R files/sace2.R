source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")

load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-log(retail$BookStores)

# create the model
bsm<-jd3_ssf_model()
# create the components and add them to the model
ssf.add(bsm, jd3_ssf_locallineartrend("ll"))
ssf.add(bsm, jd3_ssf_seasonal("s", 12, type="HarrisonStevens"))
ssf.add(bsm, jd3_ssf_noise("n", 1, fixed=TRUE))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", 0, TRUE)
ssf.add(eq, "ll")
ssf.add(eq, "s")
ssf.add(eq, "n")
ssf.add(bsm, eq)
rslt<-ssf.estimate(bsm, log(s), marginal=FALSE)
print(result(rslt, "loglikelihood"))
print(result(rslt, "parameters"))

# create the model
bsm2<-jd3_ssf_model()
# create the components and add them to the model
ssf.add(bsm2, jd3_ssf_locallineartrend("ll", 
     levelVariance = 1, fixedLevelVariance = TRUE) )
ssf.add(bsm2, jd3_ssf_seasonal("s", 12, type="HarrisonStevens", 
     variance = 1, fixed = TRUE))
ssf.add(bsm2, jd3_ssf_noise("n", 1, fixed=TRUE))
# create the equation (fix the variance to 1)
eq2<-jd3_ssf_equation("eq", 0, TRUE)
ssf.add(eq2, "ll", .01, FALSE)
ssf.add(eq2, "s", .01, FALSE)
ssf.add(eq2, "n")
ssf.add(bsm2, eq2)
rslt2<-ssf.estimate(bsm2, log(s), marginal=TRUE)
print(result(rslt2, "loglikelihood"))
print(result(rslt2, "parameters"))

# create the model
bsm3<-jd3_ssf_model()
# create the components and add them to the model
ssf.add(bsm3, jd3_ssf_locallevel("l", initial = 0) )
ssf.add(bsm3, jd3_ssf_locallineartrend("lt", levelVariance = 0, 
                                   fixedLevelVariance = TRUE) )
ssf.add(bsm3, jd3_ssf_seasonal("s", 12, type="HarrisonStevens"))
ssf.add(bsm3, jd3_ssf_noise("n", 1, fixed=TRUE))
# create the equation (fix the variance to 1)
eq3<-jd3_ssf_equation("eq", 0, TRUE)
ssf.add(eq3, "l")
ssf.add(eq3, "lt")
ssf.add(eq3, "s")
ssf.add(eq3, "n")
ssf.add(bsm3, eq3)
rslt3<-ssf.estimate(bsm3, log(s), marginal=TRUE)
print(result(rslt3, "loglikelihood"))
print(result(rslt3, "parameters"))


#result(rslt, "ssf.P0")
#result(rslt, "ssf.T")

fs<-result(rslt, "ssf.filtering.states")
ss<-result(rslt, "ssf.smoothing.states")
plot(fs[,1], type='l')
lines(ss[,1], col="red")

ll<-function(s, l){
  # create the model
  bsm2<-jd3_ssf_model()
  # create the components and add them to the model
  ssf.add(bsm2, jd3_ssf_locallineartrend("ll", 
                                     levelVariance = 1, fixedLevelVariance = TRUE,
                                     slopevariance = 0, fixedSlopeVariance = TRUE) )
  ssf.add(bsm2, jd3_ssf_seasonal("s", 12, type="Trigonometric", 
                             variance = 1, fixed = TRUE))
  ssf.add(bsm2, jd3_ssf_noise("n", 1, fixed=TRUE))
  # create the equation (fix the variance to 1)
  eq2<-jd3_ssf_equation("eq", 0, TRUE)
  ssf.add(eq2, "ll", sqrt(.000019), TRUE)
  ssf.add(eq2, "s", l, TRUE)
  ssf.add(eq2, "n", sqrt(0.000143), TRUE)
  ssf.add(bsm2, eq2)
  rslt2<-ssf.estimate(bsm2, log(s), marginal=TRUE)
  return( result(rslt2, "loglikelihood"))
}

ll2<-function(s, l){
  # create the model
  bsm2<-jd3_ssf_model()
  # create the components and add them to the model
  ssf.add(bsm2, jd3_ssf_locallineartrend("ll", 
                                     levelVariance = .000019, fixedLevelVariance = TRUE,
                                     slopevariance = 0, fixedSlopeVariance = TRUE) )
  ssf.add(bsm2, jd3_ssf_seasonal("s", 12, type="Trigonometric", 
                             variance = abs(l*l*l), fixed = TRUE))
  ssf.add(bsm2, jd3_ssf_noise("n", 0.000143, fixed=TRUE))
  # create the equation (fix the variance to 1)
  eq2<-jd3_ssf_equation("eq", 0, TRUE)
  ssf.add(eq2, "ll")
  ssf.add(eq2, "s")
  ssf.add(eq2, "n")
  ssf.add(bsm2, eq2)
  rslt2<-ssf.estimate(bsm2, log(s), marginal=FALSE)
  return( result(rslt2, "loglikelihood"))
}

