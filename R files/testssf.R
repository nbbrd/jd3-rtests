source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")
source("./R files/jd3_utility.R")


load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-ABS$X0.2.20.10.M

# create the model
model<-jd3_ssf_model()

# create the components and add them to the model
add(model, jd3_ssf_locallineartrend("ll"))
add(model, jd3_ssf_seasonal("s", 12, type="Crude"))

# create the equation (fix the variance to 1)
eq<-jd3_ssf_equation("eq", variance=1)
add(eq, "ll")
add(eq, "s")
add(model, eq)

# or
#add(model, jd3_ssf_locallineartrend("ll"))
#add(model, jd3_ssf_seasonal("s", 12, type="Crude"))
#add(model, jd3_ssf_noise("n", 1))
#eq<-jd3_ssf_equation("eq", variance=0)
#add(eq, "ll")
#add(eq, "s")
#add(eq, "n")
#add(model, eq)


#estimate the model
rslt<-estimate(model, s)

print(result(rslt, "parameters"))
print(result(rslt, "loglikelihood"))
ts.plot(result(rslt, "ssf.smoothing.cmp(0)"))

### Multivariate model 
a<-read.table("./Data/bematrix.txt", sep='\t')
mdata<-cbind(a[,1], a[,10], a[,3], a[,4])

# create the model
model2<-jd3_ssf_model()

# create the components and add them to the model
add(model2, jd3_ssf_locallineartrend("tu", 0, -1))
add(model2, jd3_ssf_locallineartrend("ty", 0, -1))
add(model2, jd3_ssf_locallevel("tpicore", 0))
add(model2, jd3_ssf_locallevel("tpi", 0))
add(model2, jd3_ssf_ar("cycle", 2, c(1, -.5), 1, 5))

# create the equations 
eq1<-jd3_ssf_equation("eq1", variance=1)
add(eq1, "tu")
add(eq1, "cycle", NULL, jd3_ssf_loading(0))
add(model2, eq1)
eq2<-jd3_ssf_equation("eq2")
add(eq2, "ty")
add(eq2, "cycle", NULL, jd3_ssf_loading(0))
add(model2, eq2)
eq3<-jd3_ssf_equation("eq3")
add(eq3, "tpicore")
add(eq3, "cycle", NULL, jd3_ssf_loading(4))
add(model2, eq3)
eq4<-jd3_ssf_equation("eq4")
add(eq4, "tpi")
add(eq4, "cycle", NULL, jd3_ssf_loading(0))
add(model2, eq4)

#estimate the model
rslt2<-estimate(model2, mdata)

print(result(rslt2, "parameters"))
print(result(rslt2, "loglikelihood"))
ts.plot(result(rslt2, "ssf.smoothing.cmp(4)"))

# create the model X2
mdatax<-cbind(a[,1], a[,10], a[,3], a[,4], a[,6], a[,7])
model3<-jd3_ssf_model()

# create the components and add them to the model
add(model3, jd3_ssf_locallineartrend("tu", 0, -1))
add(model3, jd3_ssf_locallineartrend("ty", 0, -1))
add(model3, jd3_ssf_locallevel("tpicore", 0))
add(model3, jd3_ssf_locallevel("tpi", 0))
add(model3, jd3_ssf_locallevel("tb", 0))
add(model3, jd3_ssf_locallevel("tc", 0))
add(model3, jd3_ssf_ar("cycle", 2, c(1, -.5), 1, 5))

# create the equations 
eq1<-jd3_ssf_equation("eq1", variance=1)
add(eq1, "tu")
add(eq1, "cycle", NULL, jd3_ssf_loading(0))
add(model3, eq1)
eq2<-jd3_ssf_equation("eq2")
add(eq2, "ty")
add(eq2, "cycle", NULL, jd3_ssf_loading(0))
add(model3, eq2)
eq3<-jd3_ssf_equation("eq3")
add(eq3, "tpicore")
add(eq3, "cycle", NULL, jd3_ssf_loading(4))
add(model3, eq3)
eq4<-jd3_ssf_equation("eq4")
add(eq4, "tpi")
add(eq4, "cycle", NULL, jd3_ssf_loading(0))
add(model3, eq4)
eq5<-jd3_ssf_equation("eq5")
add(eq5, "tb")
add(eq5, "cycle", NULL, jd3_ssf_loading(0))
add(eq5, "cycle", NULL, jd3_ssf_loading(1))
add(model3, eq5)
eq6<-jd3_ssf_equation("eq6")
add(eq6, "tc")
add(eq6, "cycle", NULL, jd3_ssf_loading(0))
add(eq6, "cycle", NULL, jd3_ssf_loading(1))
add(model3, eq6)

#estimate the model
rslt3<-estimate(model3, mdatax)

print(result(rslt3, "parameters"))
print(result(rslt3, "loglikelihood"))
ts.plot(result(rslt3, "ssf.smoothing.cmp(6)"))
