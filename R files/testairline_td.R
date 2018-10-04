source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")
source("./R files/jd3_utility.R")


load("./Data/retail.rda")
load("./Data/ABS.rda")

s<-ABS$X0.2.20.10.M

# create the model
airline_td<-function(s, tdgroups, contrast = FALSE){
  air<-jd3_ssf_model()
  
  # create the components and add them to the model
  add(air, jd3_ssf_sarima("airline", 12, c(0,1,1), c(0,1,1)))
  add(air, jd3_ssf_td("td", frequency(s), start(s), length(s), tdgroups, contrast, variance = .0001, fixed=FALSE))
  # create the equation (fix the variance to 1)
  eq<-jd3_ssf_equation("eq", 0, TRUE)
  add(eq, "airline")
  add(eq, "td")
  #estimate the model
  add(air, eq)
  rslt<-estimate(air, s, 1e-20, marginal=TRUE, concentrated=TRUE)
  return(rslt)
}

airline_td_fixed<-function(s, tdgroups){
  air<-jd3_ssf_model()
  
  # create the components and add them to the model
  add(air, jd3_ssf_sarima("airline", 12, c(0,1,1), c(0,1,1)))
  add(air, jd3_ssf_td("td", frequency(s), start(s), length(s), tdgroups, FALSE, variance = 0, fixed=TRUE))
  # create the equation (fix the variance to 1)
  eq<-jd3_ssf_equation("eq", 0, TRUE)
  add(eq, "airline")
  add(eq, "td")
  #estimate the model
  add(air, eq)
  rslt<-estimate(air, s, 1e-20, marginal=TRUE, concentrated=TRUE)
  return(rslt)
}


airline_td_all<-function(s, contrast = FALSE){
  rslt=airline_td(s, c(1,1,1,1,1,0,0), contrast)
  print(result(rslt, "loglikelihood"))
  print(result(rslt, "parameters"))
  rslt=airline_td(s, c(1,1,1,1,1,2,0), contrast)
  print(result(rslt, "loglikelihood"))
  print(result(rslt, "parameters"))
  rslt=airline_td(s, c(1,1,1,1,2,3,0), contrast)
  print(result(rslt, "loglikelihood"))
  print(result(rslt, "parameters"))
  rslt=airline_td(s, c(1,2,3,4,5,6,0), contrast)
  print(result(rslt, "loglikelihood"))
  print(result(rslt, "parameters"))
}
