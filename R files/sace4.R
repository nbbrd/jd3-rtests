source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")

### Multivariate model 
a<-read.table("./Data/bematrix.txt", sep='\t')
#a<-scale(a)

### normalize a. The results are much more stable

mdata<-cbind(a[,1], a[,10], a[,3], a[,4])

# create the model
model<-jd3_ssf_model()

# create the components and add them to the model
ssf.add(model, jd3_ssf_locallineartrend("tu", levelVariance = 0, fixedLevelVariance = TRUE ))
ssf.add(model, jd3_ssf_locallineartrend("ty", levelVariance = 0, fixedLevelVariance = TRUE))
ssf.add(model, jd3_ssf_locallevel("tpicore"))
ssf.add(model, jd3_ssf_locallevel("tpi"))
ssf.add(model, jd3_ssf_ar("cycle", c(1, -.5), fixedar = FALSE, variance= 1, fixedvariance=TRUE, nlags= 5))

# create the equations 
eq1<-jd3_ssf_equation("eq1")
ssf.add(eq1, "tu")
ssf.add(eq1, "cycle", .1, FALSE)
ssf.add(model, eq1)
eq2<-jd3_ssf_equation("eq2")
ssf.add(eq2, "ty")
ssf.add(eq2, "cycle", .1, FALSE)
ssf.add(model, eq2)
eq3<-jd3_ssf_equation("eq3")
ssf.add(eq3, "tpicore")
ssf.add(eq3, "cycle", .1, FALSE, jd3_ssf_loading(4))
ssf.add(model, eq3)
eq4<-jd3_ssf_equation("eq4")
ssf.add(eq4, "tpi")
ssf.add(eq4, "cycle", .1, FALSE)
ssf.add(model, eq4)

#estimate the model
rslt2<-ssf.estimate(model, mdata, marginal=T, concentrated=T)

print(result(rslt2, "parameters"))
print(result(rslt2, "loglikelihood"))
factor=sqrt(result(rslt2, "scalingfactor"))
print(factor)
