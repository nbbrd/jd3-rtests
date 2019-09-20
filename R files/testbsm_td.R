source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")
source("./R files/jd3_utility.R")


load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-ABS$X0.2.20.10.M

bsm_td<-function(s, tdgroups, contrast = FALSE){
  # create the model
  bsm<-jd3_ssf_model()
  # create the components and add them to the model
  ssf.add(bsm, jd3_ssf_locallineartrend("ll"))
  ssf.add(bsm, jd3_ssf_seasonal("s", 12, type="Crude"))
  ssf.add(bsm, jd3_ssf_td("td", frequency(s), start(s), length(s), tdgroups, contrast, variance = .0001, fixed=FALSE))
  # create the equation (fix the variance to 1)
  eq<-jd3_ssf_equation("eq", 1, TRUE)
  ssf.add(eq, "ll")
  ssf.add(eq, "s")
  ssf.add(eq, "td")
  ssf.add(bsm, eq)
  #estimate the model
  rslt<-ssf.estimate(bsm, s, marginal=T, concentrated=TRUE, precision = 1e-20)
  return(rslt)
}


bsm_td_fixed<-function(s, tdgroups, contrast = FALSE){
  # create the model
  bsm<-jd3_ssf_model()
  # create the components and add them to the model
  ssf.add(bsm, jd3_ssf_locallineartrend("ll"))
  ssf.add(bsm, jd3_ssf_seasonal("s", 12, type="Crude"))
  ssf.add(bsm, jd3_ssf_td("td", frequency(s), start(s), length(s), tdgroups, contrast, variance = 0, fixed=TRUE))
  # create the equation (fix the variance to 1)
  eq<-jd3_ssf_equation("eq", 1, TRUE)
  ssf.add(eq, "ll")
  ssf.add(eq, "s")
  ssf.add(eq, "td")
  ssf.add(bsm, eq)
  #estimate the model
  rslt<-ssf.estimate(bsm, s, marginal=T, concentrated=TRUE, precision=1e-20)
  return(rslt)
}

bsm_td_all<-function(s, contrast = FALSE){
  rslt=bsm_td(s, c(1,1,1,1,1,0,0), contrast)
  print(result(rslt, "loglikelihood"))
  print(result(rslt, "parameters"))
  rslt=bsm_td(s, c(1,1,1,1,1,2,0), contrast)
  print(result(rslt, "loglikelihood"))
  print(result(rslt, "parameters"))
  rslt=bsm_td(s, c(1,1,1,1,2,3,0), contrast)
  print(result(rslt, "loglikelihood"))
  print(result(rslt, "parameters"))
  rslt=bsm_td(s, c(1,2,3,4,5,6,0), contrast)
  print(result(rslt, "loglikelihood"))
  print(result(rslt, "parameters"))
}


bsm_td_all(log(ABS$X0.2.20.10.M), T) 