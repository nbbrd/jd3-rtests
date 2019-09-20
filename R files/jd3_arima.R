source("./R files/jd3_init.R")
source("./R files/jd3_procresults.R")
source("./R files/jd3_arimapredict.R")

setClass(
  Class="JD3_Arima",
  contains = "JD3_ProcResults"
)


setMethod("coef", "JD3_Arima", function(object){
  if (is.null(object@internal)){
    NULL
  }else{
  proc_vector(object@internal, "arima.parameters")
  }
})

setMethod("predict", "JD3_Arima", function(object, nforecasts=0, nbackcasts=0, method=c("all", "exact", "fast")){
  
  if (is.null(object@internal)){
    return (NULL)
  }else{
    method=match.arg(method)
    mean<-proc_numeric(object@internal, "mean")
    jregarima<-.jcall(object@internal, "Ljdplus/regarima/RegArimaModel;", "getRegarima")
    jrslt<-.jcall("demetra/r/ArimaForecasts", "Ldemetra/r/ArimaForecasts$Results;", "process", jregarima, mean, as.integer(nforecasts), as.integer(nbackcasts), method)
    return (new (Class = "JD3_ArimaPredict", internal = jrslt))
  }
})

setMethod("logLik", "JD3_Arima", function(object){
  if (is.null(object@internal)){
    return (NaN)
  }else{
    return (proc_numeric(object@internal,"likelihood.ll"))
  }
})

setMethod("show", "JD3_Arima", function(object){
  if (is.null(object@internal)){
    cat("Invalid estimation")
  }else{
    cat("Arima model", "\n")
    p=proc_vector(object@internal, "arima.parameters")
    cat("Coefficients: ", format(round(p, 5), scientific = FALSE), "\n")
    jmat<-proc_data(object@internal, "pcov")
    e=diag(jmat)
    cat("stdev: ", format(round(sqrt(e), 5), scientific = FALSE), "\n")
    s=proc_vector(object@internal, "score")
    cat("Scores: ", format(round(s, 5), scientific = FALSE), "\n")
    ll<-proc_numeric(object@internal,"likelihood.ll")
    cat("Log likelihood = ", format(round(ll, 4), scientific = FALSE), "\n")
  }
})

jd3_arimaspec<-function(p, d, q, period=1, bp=0, bd=0, bq=0){
  jrslt<-.jnew("demetra/sarima/SarimaSpecification")
  .jcall(jrslt, "V", "setPeriod", as.integer(period))
  .jcall(jrslt, "V", "setP", as.integer(p))
  .jcall(jrslt, "V", "setD", as.integer(d))
  .jcall(jrslt, "V", "setQ", as.integer(q))
  if (period > 1){
    .jcall(jrslt, "V", "setBP", as.integer(bp))
    .jcall(jrslt, "V", "setBD", as.integer(bd))
    .jcall(jrslt, "V", "setBQ", as.integer(bq))
  }
  jrslt
}

jd3_arima<-function(y, order=c(0L,1L,1L), seasonal=c(0L,1L,1L), xreg=NULL, mean=FALSE){
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  
  jarima=.jnew("demetra/r/ArimaEstimation")
  .jcall(jarima, "V", "setOrder", as.integer(order))
  freq=frequency(y);
  .jcall(jarima, "V", "setPeriod", as.integer(freq))
  if (freq > 1){
  .jcall(jarima, "V", "setSeasonalOrder", as.integer(seasonal))
  }
  if (mean)
    .jcall(jarima, "V", "setMean", TRUE);
  .jcall(jarima, "V", "setY", as.double(y))
  
  jrslt<-.jcall(jarima, "Ldemetra/r/ArimaEstimation$Results;", "process")
  new (Class = "JD3_Arima", internal = jrslt)
}