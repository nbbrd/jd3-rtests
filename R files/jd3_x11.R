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


jd3_x11<-function(y, period, multiplicative=TRUE, henderson=13, 
                  seas0=c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"),
                  seas1=c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"), 
                  lsigma=1.5, usigma=2.5){
  seas0=match.arg(seas0)
  seas1=match.arg(seas1)
  jrslt<-.jcall("demetra/r/X11Decomposition", "Ldemetra/r/X11Decomposition$Results;", "process", as.numeric(y), period, multiplicative, as.integer(henderson), seas0, seas1, lsigma, usigma)
  new (Class = "JD3_X11", internal = jrslt)
}

jd3_henderson<-function(y, length, musgrave=TRUE, ic=4.5){
  return (.jcall("demetra/r/X11Decomposition", "[D", "henderson", as.numeric(y), as.integer(length), musgrave, ic))
}

jd3_icratios<-function(y, t, nlags, multiplicative=TRUE){
  return (.jcall("demetra/r/X11Decomposition", "[D", "icratios", as.numeric(y), as.numeric(t), as.integer(nlags), multiplicative))
}

# See Proietti-Luati [2008] (Real time estimation in local polynomial regression...) for the terminology
jd3_localpolynomials<-function(y, horizon, degree=3, kernel=c("Henderson", "Uniform", "Biweight", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic"), endpoints=c("DAF", "LC", "QL", "CQ"), ic=4.5){
  d<-2/(sqrt(pi)*ic)
  kernel=match.arg(kernel)
  endpoints=match.arg(endpoints)
  return (.jcall("demetra/r/LocalPolynomialFilters", "[D", "filter", as.numeric(y), as.integer(horizon), as.integer(degree), kernel, endpoints, d))
}
