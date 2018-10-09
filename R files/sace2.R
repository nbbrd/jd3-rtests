source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")

load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-retail$RetailSalesTotal

# create the model
bsm<-jd3_ssf_model()
# create the components and add them to the model
add(bsm, jd3_ssf_locallineartrend("ll"))
add(bsm, jd3_ssf_seasonal("s", 12, type="HarrisonStevens"))
add(bsm, jd3_ssf_noise("n", 1, fixed=TRUE))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", 0, TRUE)
add(eq, "ll")
add(eq, "s")
add(eq, "n")
add(bsm, eq)
rslt<-estimate(bsm, log(s), marginal=TRUE)
print(result(rslt, "loglikelihood"))
print(result(rslt, "parameters"))

# create the model
bsm2<-jd3_ssf_model()
# create the components and add them to the model
add(bsm2, jd3_ssf_locallineartrend("ll", 
     levelVariance = 1, fixedLevelVariance = TRUE) )
add(bsm2, jd3_ssf_seasonal("s", 12, type="HarrisonStevens", 
     variance = 1, fixed = TRUE))
add(bsm2, jd3_ssf_noise("n", 1, fixed=TRUE))
# create the equation (fix the variance to 1)
eq2<-jd3_ssf_equation("eq", 0, TRUE)
add(eq2, "ll", .01, FALSE)
add(eq2, "s", .01, FALSE)
add(eq2, "n")
add(bsm2, eq2)
rslt2<-estimate(bsm2, log(s), marginal=TRUE)
print(result(rslt2, "loglikelihood"))
print(result(rslt2, "parameters"))

#result(rslt, "ssf.P0")
#result(rslt, "ssf.T")

fs<-result(rslt, "ssf.filtering.states")
ss<-result(rslt, "ssf.smoothing.states")
plot(fs[,1], type='l')
lines(ss[,1], col="red")