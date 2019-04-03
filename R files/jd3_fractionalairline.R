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


jd3_fractionalAirlineDecomposition<-function(y, period, adjust=TRUE, sn=F){
  jrslt<-.jcall("demetra/r/FractionalAirlineDecomposition", "Ldemetra/r/FractionalAirlineDecomposition$Results;", "process", as.numeric(y), period, adjust, sn)
  new (Class = "JD3_FractionalAirlineDecomposition", internal = jrslt)
}

setClass(
  Class="JD3_PeriodicAirline",
  contains = "JD3_ProcResults"
)

jd3_periodicAirline<-function(y, periods, x = NULL, mean = FALSE, outliers=NULL, criticalValue=6){
  if (is.null(outliers))
    joutliers<-.jnull("[S")
  else
    joutliers=.jarray(outliers, "java.lang.String")
  jrslt<-.jcall("demetra/r/PeriodicAirline", "Ldemetra/r/PeriodicAirline$Results;", "process", as.numeric(y), matrix_r2jd(x), mean, .jarray(periods), joutliers
                , criticalValue)
  new (Class = "JD3_FractionalAirlineDecomposition", internal = jrslt)
}
