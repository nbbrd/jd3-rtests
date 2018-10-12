source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")

load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-retail$BookStores
w<-runif(length(s),min=.005, max=.006)
# create the model
bsm<-jd3_ssf_model()
# create the components and add them to the model
add(bsm, jd3_ssf_locallineartrend("ll"))
add(bsm, jd3_ssf_seasonal("s", 12, type="HarrisonStevens"))
add(bsm, jd3_ssf_sae("n", c(-.5)))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", 0, TRUE)
add(eq, "ll")
add(eq, "s")
add(eq, "n", 1, TRUE, jd3_ssf_varloading(0, w))
add(bsm, eq)
rslt<-estimate(bsm, log(s), concentrated=FALSE)
print(result(rslt, "loglikelihood"))
print(result(rslt, "parameters"))
print(result(rslt, "scalingfactor"))
