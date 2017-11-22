source("./R files/jd3_init.R")
source("./R files/jd3_procresults.R")

setClass(
  Class="JD3_movingTD",
  contains = "JD3_ProcResults"
)

setMethod("logLik", "JD3_movingTD", function(object){
  if (is.null(object@internal)){
    NaN
  }else{
    proc_numeric(object@internal, "likelihood.ll")}
})

setMethod("coef", "JD3_movingTD", function(object){
  if (is.null(object@internal)){
    NULL
  }else{
    proc_vector(object@internal, "arima.parameters")}
})

setMethod("show", "JD3_movingTD", function(object){
  if (is.jnull(object@internal)){
    cat("Invalid estimation")
  }else{
    cat("Arima model", "\n")
    p=proc_vector(object@internal, "arima.parameters")
    cat("Coefficients: ", format(round(p, 5), scientific = FALSE), "\n")
    ll<-proc_numeric(object@internal,"likelihood.ll")
    cat("Log likelihood = ", format(round(ll, 4), scientific = FALSE), "\n")
  }
})


jd3_movingtd<-function(s, td="TD7", length=10){
  
  jd_s<-ts_r2jd(s)
  jrslt=.jcall("demetra/r/MovingRegression", 
               "Ldemetra/r/MovingRegression$Results;", 
               "regarima", jd_s, td, as.integer(length))
  new (Class = "JD3_movingTD", internal = jrslt)
  
}


