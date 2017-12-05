source("./R files/jd3_init.R")
source("./R files/jd3_procresults.R")

setClass(
  Class="JD3_Sts",
  contains = "JD3_ProcResults"
)


setMethod("logLik", "JD3_Sts", function(object){
  if (is.null(object@internal)){
    return (NaN)
  }else{
    return (proc_numeric(object@internal,"likelihood.ll"))
  }
})

setMethod("show", "JD3_Sts", function(object){
  if (is.null(object@internal)){
    cat("Invalid estimation")
  }else{
    cat("Structural time series", "\n")
    cat("Variances: ")
    s<-proc_numeric(object@internal, "levelvar")
    if (! is.nan(s) && s >=0) cat("level: ", format(round(s, 6), scientific = FALSE), "; ")
    s<-proc_numeric(object@internal, "slopevar")
    if (! is.nan(s) && s >=0) cat("slope: ", format(round(s, 6), scientific = FALSE), "; ")
    s<-proc_numeric(object@internal, "seasvar")
    if (! is.nan(s) && s >=0) cat("seas: ", format(round(s, 6), scientific = FALSE), "; ")
    s<-proc_numeric(object@internal, "noisevar")
    if (! is.nan(s) && s >=0) cat("noise: ", format(round(s, 6), scientific = FALSE), "\n")
    s<-proc_vector(object@internal, "score")
    cat("Scores: ", format(round(s, 5), scientific = FALSE), "\n")
    ll<-proc_numeric(object@internal,"likelihood.ll")
    cat("Log likelihood = ", format(round(ll, 4), scientific = FALSE), "\n")
  }
})

setMethod("saDecomposition", "JD3_Sts", function(object){
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



jd3_sts<-function(y, level=1, slope=1, cycle=-1, noise=1
                  , seasonal=c("Trigonometric", "Dummy", "Crude", "HarrisonStevens", "Fixed", "Unused")){
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  seasonal<-match.arg(seasonal)
  jsts=.jcall("demetra/r/StsEstimation", "Ldemetra/r/StsEstimation$Results;", "process", ts_r2jd(y), as.integer(level), as.integer(slope), as.integer(cycle), as.integer(noise), seasonal)
  new (Class = "JD3_Sts", internal = jsts)
}