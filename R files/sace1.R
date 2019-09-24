source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")


load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-ABS$X0.2.20.10.M

# create the model
bsm<-jd3_ssf_model()
# create the components and add them to the model
ssf.add(bsm, jd3_ssf_locallineartrend("ll"))
ssf.add(bsm, jd3_ssf_seasonal("s", 12, type="HarrisonStevens"))
ssf.add(bsm, jd3_ssf_td("td", frequency(s), start(s), length(s), c(1,1,1,1,2,3,0)))
# create the equation 
eq<-jd3_ssf_equation("eq", .01, F)
ssf.add(eq, "ll")
ssf.add(eq, "s")
ssf.add(eq, "td")
ssf.add(bsm, eq)
#estimate the model
rslt<-ssf.estimate(bsm, log(s), marginal=T, concentrated=T)
ss<-result(rslt, "ssf.smoothing.states")
plot(-(4*ss[,14]+ss[,15]+ss[,16]), type="l", ylim=c(-0.035,0))
print(result(rslt, "loglikelihood"))
print(result(rslt, "parameters"))
# create the model
bsm2<-jd3_ssf_model()
# create the components and add them to the model
ssf.add(bsm2, jd3_ssf_locallineartrend("ll"))
ssf.add(bsm2, jd3_ssf_seasonal("s", 12, type="HarrisonStevens"))
ssf.add(bsm2, jd3_ssf_td("td", frequency(s), start(s), length(s), c(1,2,3,4,5,6,0)))
# create the equation 
eq2<-jd3_ssf_equation("eq", .01, F)
ssf.add(eq2, "ll")
ssf.add(eq2, "s")
ssf.add(eq2, "td")
ssf.add(bsm2, eq2)
#estimate the model
rslt2<-ssf.estimate(bsm2, log(s), marginal=T, concentrated=T)
ss2<-result(rslt2, "ssf.smoothing.states")
lines(-(ss2[,14]+ss2[,15]+ss2[,16]+ss2[,17]+ss2[,18]+ss2[,19]), col="red")
print(result(rslt2, "loglikelihood"))
print(result(rslt2, "parameters"))

# create the model
airline<-jd3_ssf_model()
# create the components and add them to the model
ssf.add(airline, jd3_ssf_sarima("air", 12, c(0,1,1), c(0,1,1), fixedvariance = FALSE) )
ssf.add(airline, jd3_ssf_td("td", frequency(s), start(s), length(s), c(1,1,1,1,2,3,0)))
# create the equation (fix the variance to 0)
eq<-jd3_ssf_equation("eq", 0, TRUE)
ssf.add(eq, "air")
ssf.add(eq, "td")
ssf.add(airline, eq)
#estimate the model
arslt<-ssf.estimate(airline, log(s), marginal=T, concentrated=T)
print(result(arslt, "parameters"))
ass<-result(arslt, "ssf.smoothing.states")
plot(-(4*ass[,15]+ass[,16]+ass[,17]), type="l", ylim=c(-0.035,0))
# create the model
airline<-jd3_ssf_model()
# create the components and add them to the model
ssf.add(airline, jd3_ssf_sarima("air", 12, c(0,1,1), c(0,1,1), fixedvariance = FALSE) )
ssf.add(airline, jd3_ssf_td("td", frequency(s), start(s), length(s), c(1,1,1,1,2,3,0)))
# create the equation (fix the variance to 0)
eq<-jd3_ssf_equation("eq", 0, TRUE)
ssf.add(eq, "air")
ssf.add(eq, "td")
ssf.add(airline, eq)
arslt<-ssf.estimate(airline, log(s), marginal=T, concentrated=F)
print(result(arslt, "parameters"))
afs<-result(arslt, "ssf.filtering.states")
lines(-(4*afs[,15]+afs[,16]+afs[,17]), type="l", ylim=c(-0.035,0), col="red")



