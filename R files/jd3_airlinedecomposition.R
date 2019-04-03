source("./R files/jd3_init.R")
source("./R files/jd3_procresults.R")

setClass(
  Class="JD3_AirlineDecomposition",
  contains = "JD3_ProcResults"
)


setMethod("coef", "JD3_AirlineDecomposition", function(object){
  if (is.jnull(object@internal)){
    NULL
  }else{
    proc_vector(object@internal, "arima.parameters")
  }
})

setMethod("logLik", "JD3_AirlineDecomposition", function(object){
  if (is.jnull(object@internal)){
    return (NaN)
  }else{
    return (proc_numeric(object@internal,"likelihood.ll"))
  }
})

setMethod("show", "JD3_AirlineDecomposition", function(object){
  if (is.jnull(object@internal)){
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

setMethod("saDecomposition", "JD3_AirlineDecomposition", function(object){
  if (is.jnull(object@internal)){
    return (NULL)
  }else{
    y<-proc_ts(object@internal, "y")
    sa<-proc_ts(object@internal, "sa")
    trend<-proc_ts(object@internal, "t")
    seas<-proc_ts(object@internal, "s")
    irr<-proc_ts(object@internal, "i")
    return (ts.union(y, sa, trend, seas, irr))    
  }
})


jd3_airlineDecomposition<-function(y){
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  
  jrslt<-.jcall("demetra/r/AirlineDecomposition", "Ldemetra/r/AirlineDecomposition$Results;", "process", ts_r2jd(y), F)
  new (Class = "JD3_AirlineDecomposition", internal = jrslt)
}