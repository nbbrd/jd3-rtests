source("./R files/jd3_init.R")
source("./R files/jd3_procresults.R")

setClass(
  Class="JD3_TimeVaryingAirline",
  contains = "JD3_ProcResults"
)

setMethod("logLik", "JD3_TimeVaryingAirline", function(object){
  if (is.null(object@internal)){
    NaN
  }else{
    proc_numeric(object@internal, "likelihood.ll")}
})

setMethod("coef", "JD3_TimeVaryingAirline", function(object){
  if (is.null(object@internal)){
    NULL
  }else{
    proc_vector(object@internal, "arima.parameters")}
})

setMethod("show", "JD3_TimeVaryingAirline", function(object){
  if (is.jnull(object@internal)){
    cat("Invalid estimation")
  }else{
    cat("Model", "\n")
    p<-proc_vector(object@internal, "arima.parameters")
    tdvar<-proc_numeric(object@internal,"tdvar")
    cat("Arima coefficients: ", format(round(p, 5), scientific = FALSE), "\n")
    cat("TD variance: ", format(round(tdvar, 9), scientific = FALSE), "\n")
    ll0<-proc_numeric(object@internal,"likelihood0.ll")
    aic0<-proc_numeric(object@internal,"aic0")
    cat("Log likelihood for model with fixed TD = ", format(round(ll0, 4), scientific = FALSE))
    cat(", AIC = ", format(round(aic0, 4), scientific = FALSE), "\n")
    ll<-proc_numeric(object@internal,"likelihood.ll")
    aic<-proc_numeric(object@internal,"aic")
    cat("Log likelihood for model with time-varying TD= ", format(round(ll, 4), scientific = FALSE))
    cat(", AIC = ", format(round(aic, 4), scientific = FALSE), "\n")
  }
})


jd3_tdvar<-function(s, td="TD7", var="Default", aicdiff=0){
  
  jd_s<-ts_r2jd(s)
  jrslt<-.jcall("demetra/r/TimeVaryingRegression", 
               "Ldemetra/r/TimeVaryingRegression$Results;", 
               "regarima", jd_s, td, var, aicdiff)
  new (Class = "JD3_TimeVaryingAirline", internal = jrslt)
  
}



