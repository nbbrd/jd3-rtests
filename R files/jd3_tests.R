source("./R files/jd3_init.R")
source("./R files/jd3_rslts.R")

load("./Data/retail.rda")
load("./Data/ABS.rda")

jd3_seasonality_FTest<-function(series, ar=TRUE, nyears=0){
  js<-ts_r2jd(series)
  jtest<-.jcall("demetra/r/SeasonalityTests", "Ldemetra/stats/TestResult;", "ftest", js, ar, as.integer(nyears))
  if (is.jnull(jtest))
    return (NULL)
  else{
    desc<-.jcall(jtest, "S", "getDescription")
    val<-.jcall(jtest, "D", "getValue")
    pval<-.jcall(jtest, "D", "getPvalue")
    all<-c(val, pval)
    attr(all, "description")<-desc
    return (all)
  }
}

jd3_seasonality_QSTest<-function(series, nyears=0){
  js<-ts_r2jd(series)
  jtest<-.jcall("demetra/r/SeasonalityTests", "Ldemetra/stats/TestResult;", "qstest", js, as.integer(nyears))
  if (is.jnull(jtest))
    return (NULL)
  else{
    desc<-.jcall(jtest, "S", "getDescription")
    val<-.jcall(jtest, "D", "getValue")
    pval<-.jcall(jtest, "D", "getPvalue")
    all<-c(val, pval)
    attr(all, "description")<-desc
    return (all)
  }
}

jd3_td_FTest<-function(series, ar=TRUE, nyears=0){
  js<-ts_r2jd(series)
  jtest<-.jcall("demetra/r/TradingDaysTests", "Ldemetra/stats/TestResult;", "ftest", js, ar, as.integer(nyears))
  if (is.jnull(jtest))
    return (NULL)
  else{
    desc<-.jcall(jtest, "S", "getDescription")
    val<-.jcall(jtest, "D", "getValue")
    pval<-.jcall(jtest, "D", "getPvalue")
    all<-c(val, pval)
    attr(all, "description")<-desc
    return (all)
  }
}