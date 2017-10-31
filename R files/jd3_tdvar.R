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
  proc_vector(object@internal, "arima.rparameters")}
})

jd3_tdvar<-function(s, td="TD7", var="Default"){
  
  jd_s=ts_r2jd(s)
  jrslt=.jcall("demetra/r/TimeVaryingRegression", 
               "Ldemetra/r/TimeVaryingRegression$Results;", 
               "regarima", jd_s, td, var)
  new (Class = "JD3_TimeVaryingAirline", internal = jrslt)
  
}


