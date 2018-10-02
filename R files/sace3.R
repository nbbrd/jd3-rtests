source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")
source("./R files/jd3_utility.R")


load("./Data/retail.rda")

# create the model
bsm_td_periodic<-function(s, tdgroups, period, noisyperiod, contrast = FALSE){
  bsm<-jd3_ssf_model()
  
  # create the components and add them to the model
  add(bsm, jd3_ssf_locallineartrend("ll"))
  add(bsm, jd3_ssf_seasonal("s", 12, type="Crude"))
  add(bsm, jd3_ssf_td("td", frequency(s), start(s), length(s), tdgroups, contrast, variance = 0, fixed=TRUE))
  add(bsm, jd3_ssf_noise("n", 1, TRUE))
  for (i in 1:length(noisyperiod)){
    add(bsm, jd3_ssf_noise(paste("pn", i, sep=""), variance = .01, fixed=FALSE))
  }
  # create the equation (fix the variance to 1)
  eq<-jd3_ssf_equation("eq", 0, TRUE)
  add(eq, "ll")
  add(eq, "s")
  add(eq, "td")
  add(eq, "n")
  for (i in 1:length(noisyperiod)){
    add(eq,paste("pn", i, sep=""), 1, TRUE, jd3_ssf_loading_periodic(period, noisyperiod[i]))
  }
  add(bsm, eq)
  #estimate the model
  rslt<-estimate(bsm, s, 1e-20, marginal=TRUE, concentrated=TRUE)
  return(rslt)
}
