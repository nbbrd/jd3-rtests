source("./R files/jd3_init.R")
source("./R files/jd3_procresults.R")

setClass(
  Class="JD3_FractionalAirlineDecomposition",
  contains = "JD3_ProcResults"
)


setMethod("coef", "JD3_FractionalAirlineDecomposition", function(object){
  if (is.jnull(object@internal)){
    NULL
  }else{
    proc_vector(object@internal, "parameters")
  }
})

setMethod("logLik", "JD3_FractionalAirlineDecomposition", function(object){
  if (is.jnull(object@internal)){
    return (NaN)
  }else{
    return (proc_numeric(object@internal,"likelihood.ll"))
  }
})

setMethod("show", "JD3_FractionalAirlineDecomposition", function(object){
  if (is.jnull(object@internal)){
    cat("Invalid estimation")
  }else{
    cat("Arima model", "\n")
    p=proc_vector(object@internal, "parameters")
    cat("Coefficients: ", format(round(p, 5), scientific = FALSE), "\n")
    ll<-proc_numeric(object@internal,"likelihood.ll")
    cat("Log likelihood = ", format(round(ll, 4), scientific = FALSE), "\n")
  }
})

setMethod("saDecomposition", "JD3_FractionalAirlineDecomposition", function(object){
  if (is.jnull(object@internal)){
    return (NULL)
  }else{
    y<-proc_data(object@internal, "y")
    sa<-proc_data(object@internal, "sa")
    trend<-proc_data(object@internal, "t")
    seas<-proc_data(object@internal, "s")
    irr<-proc_data(object@internal, "i")
    return (cbind(y, sa, trend, seas, irr))    
  }
})


jd3_fractionalAirlineDecomposition<-function(y, period, adjust=TRUE){
  jrslt<-.jcall("demetra/r/FractionalAirlineDecomposition", "Ldemetra/r/FractionalAirlineDecomposition$Results;", "process", as.numeric(y), period, adjust)
  new (Class = "JD3_FractionalAirlineDecomposition", internal = jrslt)
}