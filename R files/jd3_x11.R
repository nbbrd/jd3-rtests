source("./R files/jd3_init.R")
source("./R files/jd3_procresults.R")

setClass(
  Class="JD3_X11",
  contains = "JD3_ProcResults"
)



setMethod("show", "JD3_X11", function(object){
  if (is.jnull(object@internal)){
    cat("Invalid estimation")
  }else{
    cat("X11", "\n")
  }
})

setMethod("predict", "JD3_X11", function(object){
  if (is.jnull(object@internal)){
    cat("Invalid estimation")
  }else{
    cat("X11", "\n")
  }
})

setMethod("saDecomposition", "JD3_X11", function(object){
  if (is.jnull(object@internal)){
    return (NULL)
  }else{
    y<-proc_data(object@internal, "y")
    sa<-proc_data(object@internal, "d11")
    trend<-proc_data(object@internal, "d12")
    seas<-proc_data(object@internal, "d10")
    irr<-proc_data(object@internal, "d13")
    return (cbind(y, sa, trend, seas, irr))    
  }
})


jd3_x11<-function(y, period, multiplicative=TRUE, henderson=13, seas0="S3X3", seas1="S3X5"){
  jrslt<-.jcall("demetra/r/X11Decomposition", "Ldemetra/r/X11Decomposition$Results;", "process", as.numeric(y), period, multiplicative, as.integer(henderson), seas0, seas1)
  new (Class = "JD3_X11", internal = jrslt)
}

jd3_henderson<-function(y, length, musgrave=TRUE, ic=4.5){
  return (.jcall("demetra/r/X11Decomposition", "[D", "henderson", as.numeric(y), as.integer(length), musgrave, ic))
}

jd3_icratios<-function(y, t, nlags, multiplicative=TRUE){
  return (.jcall("demetra/r/X11Decomposition", "[D", "icratios", as.numeric(y), as.numeric(t), as.integer(nlags), multiplicative))
}