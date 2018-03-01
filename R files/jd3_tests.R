source("./R files/jd3_init.R")
source("./R files/jd3_rslts.R")
source("./R files/jd3_airlinedecomposition.R")

load("./Data/retail.rda")
load("./Data/ABS.rda")

jd3_seasonality_FTest<-function(series, period = 0, ar=TRUE, nyears=0){
  p<-period
  if (p == 0){
    p<-frequency(series)
  }
  jtest<-.jcall("demetra/r/SeasonalityTests", "Ldemetra/stats/TestResult;", "fTest", as.numeric(series), as.integer(p), ar, as.integer(nyears))
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

jd3_seasonality_QSTest<-function(series, period = 0, nyears=0){
  p<-period
  if (p == 0){
    p<-frequency(series)
  }
  jtest<-.jcall("demetra/r/SeasonalityTests", "Ldemetra/stats/TestResult;", "qsTest", as.numeric(series), as.integer(p), as.integer(nyears))
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

jd3_seasonality_periodcQSTest<-function(series, periods){
  jtest<-.jcall("demetra/r/SeasonalityTests", "Ldemetra/stats/TestResult;", "periodicQsTest", as.numeric(series), as.numeric(periods))
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

jd3_ch<-function(series, startPeriodicity=2, endPeriodicity, original=TRUE){
  q<-.jcall("demetra/r/SeasonalityTests", "[D", "canovaHansenTest", as.numeric(series), as.integer(startPeriodicity), as.integer(endPeriodicity), original)  
  return(q)
}

test_td<-function(series, td="TD7", var="Default", n){
  t<-result(jd3_tdvar(series, td, var), "tdeffect")
  decomp<-jd3_airlineDecomposition(series-t)
  show(jd3_td_FTest(result(decomp, "i"), nyears=n))
  show(jd3_td_FTest(result(decomp, "sa"), nyears=n))
}