source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")
source("./R files/jd3_utility.R")


load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-ABS$X0.2.20.10.M

# create the model
bsm<-jd3_ssf_model()

# create the components and add them to the model
add(bsm, jd3_ssf_locallineartrend("ll"))
add(bsm, jd3_ssf_seasonal("s", 12, type="Crude"))
add(bsm, jd3_ssf_noise("n", variance = 1, fixed = TRUE))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", variance=0, fixed = TRUE)
add(eq, "ll")
add(eq, "s")
add(eq, "n")
add(bsm, eq)
#estimate the model
rslt<-estimate(bsm, s)
print(result(rslt, "parameters"))
print(result(rslt, "loglikelihood"))
print(result(rslt, "scalingfactor"))

# or

# create the model
bsm<-jd3_ssf_model()

# create the components and add them to the model
add(bsm, jd3_ssf_locallineartrend("ll"))
add(bsm, jd3_ssf_seasonal("s", 12, type="Crude"))
add(bsm, jd3_ssf_noise("n"))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", variance=0, fixed = TRUE)
add(eq, "ll")
add(eq, "s")
add(eq, "n")
add(bsm, eq)
#estimate the model
rslt<-estimate(bsm, s, concentrated=FALSE)
print(result(rslt, "parameters"))
rsltbis<-compute(bsm, s, result(rslt, "parameters"),concentrated=TRUE)
print(result(rslt, "loglikelihood"))
print(result(rsltbis, "scalingfactor"))
print(result(rsltbis, "loglikelihood"))


# or
bsmbis<-jd3_ssf_model()

# create the components and add them to the model
add(bsmbis, jd3_ssf_locallineartrend("ll", levelVariance = 1, fixedLevelVariance = TRUE))
add(bsmbis, jd3_ssf_seasonal("s", 12, type="Crude", 1, TRUE))
add(bsmbis, jd3_ssf_ar("ar", c(1,-.5), variance = 1, fixedvariance = TRUE))
# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", variance=0, fixed = TRUE)
add(eq, "ll", 1, TRUE)
add(eq, "s", .1, FALSE)
add(eq, "ar", .1, FALSE)
add(bsmbis, eq)
#estimate the model
rsltbis<-estimate(bsmbis, s)
print(result(rsltbis, "parameters"))
print(result(rsltbis, "loglikelihood"))
print(result(rsltbis, "scalingfactor"))


# or
bsm2<-jd3_ssf_model()

add(bsm2, jd3_ssf_locallineartrend("ll"))
add(bsm2, jd3_ssf_seasonal("s", 12, type="Crude"))
add(bsm2, jd3_ssf_noise("n", 1, TRUE))
eq<-jd3_ssf_equation("eq", variance=0, fixed = TRUE)
add(eq, "ll")
add(eq, "s")
add(eq, "n")
add(bsm2, eq)
#estimate the model
rslt2<-estimate(bsm2, s)
print(result(rslt2, "parameters"))
print(result(rslt2, "loglikelihood"))
print(result(rslt2, "scalingfactor"))



### Multivariate model 
a<-read.table("./Data/bematrix.txt", sep='\t')
mdata<-cbind(a[,1], a[,10], a[,3], a[,4])

# create the model
model2<-jd3_ssf_model()

# create the components and add them to the model
add(model2, jd3_ssf_locallineartrend("tu", levelVariance = 0, fixedLevelVariance = TRUE ))
add(model2, jd3_ssf_locallineartrend("ty", levelVariance = 0, fixedLevelVariance = TRUE))
add(model2, jd3_ssf_locallevel("tpicore"))
add(model2, jd3_ssf_locallevel("tpi"))
add(model2, jd3_ssf_ar("cycle", c(1, -.5), fixedar = FALSE, variance= 1, fixedvariance=TRUE, nlags= 5))

# create the equations 
eq1<-jd3_ssf_equation("eq1", variance=1, fixed = TRUE)
add(eq1, "tu")
add(eq1, "cycle", .1, FALSE)
add(model2, eq1)
eq2<-jd3_ssf_equation("eq2")
add(eq2, "ty")
add(eq2, "cycle", 1, FALSE)
add(model2, eq2)
eq3<-jd3_ssf_equation("eq3")
add(eq3, "tpicore")
add(eq3, "cycle", .1, FALSE, jd3_ssf_loading(4))
add(model2, eq3)
eq4<-jd3_ssf_equation("eq4")
add(eq4, "tpi")
add(eq4, "cycle", .1, FALSE)
add(model2, eq4)

#estimate the model
rslt2<-estimate(model2, mdata, marginal=TRUE, concentrated=TRUE)
#dictionary(rslt2)

print(result(rslt2, "parameters"))
print(result(rslt2, "loglikelihood"))
factor=sqrt(result(rslt2, "scalingfactor"))
print(factor)
ts.plot(result(rslt2, "ssf.smoothing.cmp(4)"))
p<-result(rslt2, "parameters")
rslt2bis<-compute(model2, mdata, p, concentrated=FALSE)
print(result(rslt2bis, "loglikelihood"))

# create the model X2
mdatax<-cbind(a[,1], a[,10], a[,3], a[,4], a[,6], a[,7])
model3<-jd3_ssf_model()

# create the components and add them to the model
add(model3, jd3_ssf_locallineartrend("tu", levelVariance = 0, fixedLevelVariance = TRUE ))
add(model3, jd3_ssf_locallineartrend("ty", levelVariance = 0, fixedLevelVariance = TRUE))
add(model3, jd3_ssf_locallevel("tpicore"))
add(model3, jd3_ssf_locallevel("tpi"))
add(model3, jd3_ssf_locallevel("tb", variance = 0, fixed = TRUE))
add(model3, jd3_ssf_locallevel("tc", variance = 0, fixed = TRUE))
add(model3, jd3_ssf_ar("cycle", c(1, -.5), fixedar = FALSE, variance= 1,fixedvariance=TRUE, nlags= 5))

# create the equations 
eq1<-jd3_ssf_equation("eq1", 1, TRUE)
add(eq1, "tu")
add(eq1, "cycle", .1, FALSE)
add(model3, eq1)
eq2<-jd3_ssf_equation("eq2")
add(eq2, "ty")
add(eq2, "cycle", .1, FALSE)
add(model3, eq2)
eq3<-jd3_ssf_equation("eq3")
add(eq3, "tpicore")
add(eq3, "cycle", .1, FALSE, jd3_ssf_loading(4))
add(model3, eq3)
eq4<-jd3_ssf_equation("eq4")
add(eq4, "tpi")
add(eq4, "cycle", .1, FALSE)
add(model3, eq4)
eq5<-jd3_ssf_equation("eq5")
add(eq5, "tb")
add(eq5, "cycle", .1, FALSE)
add(eq5, "cycle", .1, FALSE, jd3_ssf_loading(1))
add(model3, eq5)
eq6<-jd3_ssf_equation("eq6")
add(eq6, "tc")
add(eq6, "cycle", .1, FALSE)
add(eq6, "cycle", .1, FALSE, jd3_ssf_loading(1))
add(model3, eq6)

#estimate the model
rslt3<-estimate(model3, mdatax, 1e-20, marginal=TRUE, concentrated=TRUE)

print(result(rslt3, "parameters"))
factor<-sqrt((result(rslt3, "scalingfactor")))
lines(result(rslt3, "ssf.smoothing.cmp(6)"), col="red")
p<-result(rslt3, "parameters")
rslt3bis<-compute(model3, mdatax, p, concentrated=TRUE)
print(factor)
print(result(rslt3, "loglikelihood"))
print(result(rslt3bis, "loglikelihood"))

# create the model X
mdatax<-cbind(a[,1], a[,10], a[,3], a[,4], a[,6], a[,7])
model4<-jd3_ssf_model()

# create the components and add them to the model
add(model4, jd3_ssf_locallineartrend("tu", levelVariance = 0, fixedLevelVariance = TRUE ))
add(model4, jd3_ssf_locallineartrend("ty", levelVariance = 0, fixedLevelVariance = TRUE))
add(model4, jd3_ssf_locallevel("tpicore"))
add(model4, jd3_ssf_locallevel("tpi"))
add(model4, jd3_ssf_locallevel("tb", variance = 0, fixed = TRUE))
add(model4, jd3_ssf_locallevel("tc", variance = 0, fixed = TRUE))
add(model4, jd3_ssf_ar2("cycle", c(1, -.5), fixedar = FALSE, variance= 1,fixedvariance=TRUE, nlags= 4, nfcasts = 4))
# create the equations 
eq1<-jd3_ssf_equation("eq1", 1, TRUE)
add(eq1, "tu")
add(eq1, "cycle", .1, FALSE, jd3_ssf_loading(4)) # t
add(model4, eq1)
eq2<-jd3_ssf_equation("eq2")
add(eq2, "ty")
add(eq2, "cycle", .1, FALSE, jd3_ssf_loading(4))
add(model4, eq2)
eq3<-jd3_ssf_equation("eq3")
add(eq3, "tpicore")
add(eq3, "cycle", .1, FALSE, jd3_ssf_loading(0)) #t-4
add(model4, eq3)
eq4<-jd3_ssf_equation("eq4")
add(eq4, "tpi")
add(eq4, "cycle", .1, FALSE, jd3_ssf_loading(4))
add(model4, eq4)
eq5<-jd3_ssf_equation("eq5")
add(eq5, "tb")
add(eq5, "cycle", .1, FALSE, jd3_ssf_loading(5))
add(model4, eq5)
eq6<-jd3_ssf_equation("eq6")
add(eq6, "tc")
add(eq6, "cycle", .1, FALSE, jd3_ssf_loading(c(5,6,7,8), c(1,1,1,1)))
add(model4, eq6)

#estimate the model
rslt4<-estimate(model4, mdatax, marginal=TRUE, concentrated=TRUE)

print(result(rslt4, "parameters"))
print(result(rslt4, "loglikelihood"))
pos<-result(rslt4, "ssf.cmppos")
factor=sqrt(result(rslt4, "scalingfactor"))
print(result(rslt4, "parametersname"))
print(factor)

lines(result(rslt4, paste("ssf.smoothing.array(",pos[7]+4, ")", sep="")), col="blue")
lines(result(rslt4, paste("ssf.filtering.array(",pos[7]+4, ")", sep="")), col="magenta")

p<-result(rslt4, "parameters")
