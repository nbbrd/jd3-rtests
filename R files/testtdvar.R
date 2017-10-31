source("./R files/jd3_init.R")
source("./R files/jd3_tdvar.R")

load("./Data/ABS.rda")

tdvar_all<-function(s, log=TRUE, td="TD7", var="Default"){
  if (log){
    s<-log(s)
  }
  x<-jd3_tdvar(s, td, var)
  cat(coef(x), "\n")
  cat(logLik(x), "\n")
  c<-result(x, "coefficients.value")
  ec<-result(x, "coefficients.stde")
  w<-c[,dim(c)[2]]
  ew<-ec[,dim(c)[2]]
  
  plot(w, type="l", col="red", ylim=c(min(w-ew), max(w+ew)))
  lines(w+ew, col="gray")
  lines(w-ew, col="gray")
  
  plot(c[,1], type="l", ylim=c(min(c), max(c)))
  clrs<-c( "black","red", "green3","blue","cyan","magenta","gray")
  for (i in 2: dim(c)[2]){
    lines(c[,i],col=clrs[i])
  }
}

tdvar_all(ABS$X0.2.20.10.M)
tdvar_all(ABS$X0.2.20.10.M, var="Contrasts")


