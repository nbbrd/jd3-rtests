source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")
source("./R files/jd3_utility.R")



### Multivariate model 
a<-read.table("./Data/bematrix.txt", sep='\t')
#a<-scale(a)

### normalize a. The results are much more stable

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
eq1<-jd3_ssf_equation("eq1", 1, TRUE)
add(eq1, "tu")
add(eq1, "cycle", .1, FALSE)
add(model2, eq1)
eq2<-jd3_ssf_equation("eq2")
add(eq2, "ty")
add(eq2, "cycle", .1, FALSE)
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
rslt2<-estimate(model2, mdata, marginal=FALSE, concentrated=TRUE)
#dictionary(rslt2)

print(result(rslt2, "parameters"))
print(result(rslt2, "loglikelihood"))
factor=sqrt(result(rslt2, "scalingfactor"))
print(factor)
ts.plot(result(rslt2, "ssf.smoothing.cmp(4)"))

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
rslt3<-estimate(model3, mdatax, marginal=FALSE, concentrated=TRUE)

print(result(rslt3, "parameters"))
factor<-sqrt((result(rslt3, "scalingfactor")))
print(factor)
print(result(rslt3, "loglikelihood"))
lines(result(rslt3, "ssf.smoothing.cmp(6)"), col="red")

# create the model X
mdatax<-cbind(a[,1], a[,10], a[,3], a[,4], a[,6], a[,7])

for (k in 60:nrow(mdatax)){
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
if (is.null(p)){
rslt4<-estimate(model4, mdatax[1:k,], marginal=FALSE, concentrated=TRUE)
p<-result(rslt4, "parameters")
}else{
  rslt4<-estimate(model4, mdatax[1:k,], marginal=FALSE, concentrated=TRUE, initialParameters=p)
  p<-result(rslt4, "parameters")
}

if (k==60)
  plot(result(rslt4, "ssf.smoothing.cmp(1)"), type="l", xlim=c(0, 95), ylim=c(11.1, 11.6))
else
  lines(result(rslt4, "ssf.smoothing.cmp(1)"), col="blue")

}

print(result(rslt4, "parameters"))
print(result(rslt4, "loglikelihood"))
pos<-result(rslt4, "ssf.cmppos")
factor=sqrt(result(rslt4, "scalingfactor"))
print(result(rslt4, "parametersname"))
print(factor)

lines(result(rslt4, paste("ssf.filtering.array(",pos[7]+4, ")", sep="")), col="magenta")


plotcmp<-function(rslts, cmp){
  v<-result(rslts, paste("ssf.smoothing.cmp(",cmp, ")", sep=""))
  ev<-result(rslts, paste("ssf.smoothing.vcmp(",cmp, ")", sep=""))
  ev<-sqrt(ev)
  
  plot(v, type="l", ylim=c(min(v)-2*max(ev), max(v)+2*max(ev)))
  lines(v+2*ev, col="red")
  lines(v-2*ev, col="red")
}


