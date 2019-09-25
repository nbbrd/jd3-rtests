source("./R files/jd3_init.R")
source("./R files/jd3_ssf.R")

a<-read.csv("./Data/abs1.csv")

sutse<-function(s, t, concentrated = TRUE){

  model<-jd3_ssf_model()

  # create the components and add them to the model
  ssf.add(model, jd3_ssf_locallevel("l1", initial = 0))
  ssf.add(model, jd3_ssf_locallineartrend("lt1", levelVariance = 0, fixedLevelVariance = T))
  ssf.add(model, jd3_ssf_seasonal("s1", 12, type="Trigonometric"))
  ssf.add(model, jd3_ssf_noise("n1"))
  ssf.add(model, jd3_ssf_locallevel("l2", initial = 0))
  ssf.add(model, jd3_ssf_locallineartrend("lt2", levelVariance = 0, fixedLevelVariance = T))
  ssf.add(model, jd3_ssf_seasonal("s2", 12, type="Trigonometric"))
  ssf.add(model, jd3_ssf_noise("n2"))
  
  # create the equation (fix the variance to 1)
  eq1<-jd3_ssf_equation("eq1", 0, T)
  ssf.add(eq1, "l1")
  ssf.add(eq1, "lt1")
  ssf.add(eq1, "s1")
  ssf.add(eq1, "n1")
  ssf.add(model, eq1)
  eq2<-jd3_ssf_equation("eq2", 0, T)
  ssf.add(eq2, "l2")
  ssf.add(eq2, "l1", 0, F)
  ssf.add(eq2, "lt2")
  ssf.add(eq2, "lt1", 0, F)
  ssf.add(eq2, "s2")
  ssf.add(eq2, "s1", 0, F)
  ssf.add(eq2, "n2")
  ssf.add(eq2, "n1", 0, F)
  ssf.add(model, eq2)
  rslt<-ssf.estimate(model, cbind(s,t), marginal=T, concentrated=concentrated)
  return (rslt)
}

printSutse<-function(rslt){
  p<-result(rslt, "parameters")
  s<-result(rslt, "scalingfactor")
  ll<-result(rslt, "loglikelihood")
  cat("loglikelihood =", ll, "\n\n")
  cat("variable 1:\n")
  cat("level: ", s*p[1], "\n")
  cat("slope: ", s*p[3], "\n")
  cat("seasonal: ", s*p[4], "\n")
  cat("noise: ", s*p[5], "\n")
  cat("\nvariable 2:\n")
  cl<-p[11]
  csl<-p[12]
  cs<-p[13]
  cn<-p[14]
  cat("level: ", s*(p[1]*cl*cl+p[6]), "\n")
  cat("slope: ", s*(p[3]*csl*csl+p[8]), "\n")
  cat("seasonal: ", s*(p[4]*cs*cs+p[9]), "\n")
  cat("noise: ", s*(p[5]*cn*cn+p[10]), "\n")
  
  cat("\ncorrelations:\n")
  if (p[1]==0){
    cat("levels: 0", "\n")
  }else if (p[6] == 0){
    cat("levels: 1", "\n")
  }else{
    cat("levels: ",sign(cl)/(sqrt(1+p[6]/(cl*cl*p[1]))),"\n")
  }
  if (p[3]==0){
    cat("slopes: 0", "\n")
  }else if (p[8] == 0){
    cat("slopes: 1", "\n")
  }else{
    cat("slopes: ",sign(csl)/(sqrt(1+p[8]/(csl*csl*p[3]))),"\n")
  }
  if (p[4]==0){
    cat("seasonals: 0", "\n")
  }else if (p[9] == 0){
    cat("seasonals: 1", "\n")
  }else{
    cat("seasonals: ",sign(cs)/(sqrt(1+p[9]/(cs*cs*p[4]))),"\n")
  }
  if (p[5]==0){
    cat("noises: 0", "\n")
  }else if (p[10] == 0){
    cat("noises: 1", "\n")
  }else{
    cat("noises: ",sign(cn)/(sqrt(1+p[10]/(cn*cn*p[5]))),"\n")
  }
}


c1=43
c2=c1+5
#c3=c1+10

q<-sutse(a[,c1], a[,c2], T)
printSutse(q)

