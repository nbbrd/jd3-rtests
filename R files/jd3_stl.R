source("./R files/jd3_init.R")
source("./R files/jd3_procresults.R")

setClass(
  Class="JD3_STL",
  contains = "JD3_ProcResults"
)



setMethod("show", "JD3_STL", function(object){
  if (is.jnull(object@internal)){
    cat("Invalid estimation")
  }else{
    cat("STL plus", "\n")
  }
})

setMethod("saDecomposition", "JD3_STL", function(object){
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


jd3_stl<-function(y, period, multiplicative=TRUE, swindow=7, robust=TRUE){
  jrslt<-.jcall("demetra/r/StlDecomposition", "Ldemetra/r/StlDecomposition$Results;", "process", as.numeric(y), as.integer(period), multiplicative, as.integer(swindow), robust)
  new (Class = "JD3_STL", internal = jrslt)
}

jd3_loess<-function(y, window, degree=1, jump=1){
  if (degree != 0 && degree != 1)
    stop("Unsupported degree")
  if (jump <1)
    stop("jump should be greater then 0")
  return (.jcall("demetra/r/StlDecomposition", "[D", "loess", as.numeric(y), as.integer(window), as.integer(degree), as.integer(jump)))
}

