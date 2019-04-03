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
add(model, jd3_ssf_locallineartrend("tu", levelVariance = 0, fixedLevelVariance = TRUE ))
add(model, jd3_ssf_locallineartrend("ty", levelVariance = 0, fixedLevelVariance = TRUE))
add(model, jd3_ssf_locallevel("tpicore"))
add(model, jd3_ssf_locallevel("tpi"))
add(model, jd3_ssf_ar("cycle", c(1, -.5), fixedar = FALSE, variance= 1, fixedvariance=TRUE, nlags= 5))

# create the equations 
eq1<-jd3_ssf_equation("eq1")
add(eq1, "tu")
add(eq1, "cycle", .1, FALSE)
add(model, eq1)
eq2<-jd3_ssf_equation("eq2")
add(eq2, "ty")
add(eq2, "cycle", .1, FALSE)
add(model, eq2)
eq3<-jd3_ssf_equation("eq3")
add(eq3, "tpicore")
add(eq3, "cycle", .1, FALSE, jd3_ssf_loading(4))
add(model, eq3)
eq4<-jd3_ssf_equation("eq4")
add(eq4, "tpi")
add(eq4, "cycle", .1, FALSE)
add(model, eq4)

#estimate the model
rslt2<-estimate(model, mdata, marginal=T, concentrated=T)

print(result(rslt2, "parameters"))
print(result(rslt2, "loglikelihood"))
factor=sqrt(result(rslt2, "scalingfactor"))
print(factor)
ts.plot(-result(rslt2, "ssf.smoothing.cmp(4)"))
