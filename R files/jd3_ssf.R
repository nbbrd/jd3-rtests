source("./R files/jd3_init.R")
source("./R files/jd3_procresults.R")
source("./R files/jd3_ts.R")

setClass(
  Class="JD3_Ssf",
  contains = "JD3_ProcResults"
)

if (! isGeneric("compute")){
  setGeneric(name="compute", def = function( object, ...){standardGeneric("compute")})
  lockBinding("compute", .GlobalEnv)
}

if (! isGeneric("estimate")){
  setGeneric(name="estimate", def = function( object,...){standardGeneric("estimate")})
  lockBinding("estimate", .GlobalEnv)
}

if (! isGeneric("add")){
  setGeneric(name="add", def = function( object, item, ...){standardGeneric("add")})
  lockBinding("add", .GlobalEnv)
}

setClass(
  Class="JD3_SsfModel",
  contains = "JD3_Object"
)

setClass(
  Class="JD3_SsfItem",
  contains = "JD3_Object"
)

setClass(
  Class="JD3_SsfEquation",
  contains = "JD3_SsfItem"
)

setClass(
  Class="JD3_SsfDynamics",
  contains = "JD3_Object"
)

setClass(
  Class="JD3_SsfInitialization",
  contains = "JD3_Object"
)

setClass(
  Class="JD3_SsfLoading",
  contains = "JD3_Object"
)

setMethod("add", signature = c(object="JD3_SsfModel", item = "JD3_SsfItem"), function(object, item){
  if ( is.jnull(object@internal) || is.jnull(item@internal) ){
    return
  }else{
    .jcall(object@internal, "V", "add", item@internal)
  }
})

setMethod("estimate", signature = c(object="JD3_SsfModel"), function(object, data, precision=1e-15, marginal=FALSE){
  if ( is.jnull(object@internal) ){
    return(NULL)
  }else{
    jdata<-matrix_r2jd(data)
    jrslt<-.jcall(object@internal, "Lrssf/CompositeModel$Estimation;", "estimate", jdata, precision, marginal)
    return( new(Class= "JD3_ProcResults", internal=jrslt))
  }
})

setMethod("add", signature = c(object="JD3_SsfEquation", item="character"), function(object, item, coeff=1, fixed=TRUE, loading=NULL){
  if (is.null(loading))
    .jcall(object@internal, "V", "add", item, coeff, as.logical(fixed), .jnull("demetra/ssf/ISsfLoading"))
  else
    .jcall(object@internal, "V", "add", item, coeff, as.logical(fixed), loading@internal)
  
})

jd3_ssf_ar<-function(name, ar, fixedar=FALSE, variance=.01, fixedvariance=FALSE, nlags=0){
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "ar", name, .jarray(ar), fixedar, variance, fixedvariance, as.integer(nlags))
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_ar2<-function(name, ar, fixedar=FALSE, variance=.01, fixedvariance=FALSE, nlags=0, nfcasts=0){
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "ar", name, .jarray(ar), fixedar, variance, fixedvariance, as.integer(nlags), as.integer(nfcasts))
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_locallevel<-function(name, variance=.01, fixed=FALSE){
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "localLevel", name, variance, fixed)
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_locallineartrend<-function(name, levelVariance=.01, slopevariance=.01, fixedLevelVariance=FALSE, fixedSlopeVariance=FALSE ){
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "localLinearTrend", name, levelVariance, slopevariance, fixedLevelVariance, fixedSlopeVariance)
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_seasonal<-function(name, period, type="Crude", variance=.01, fixed=FALSE){
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "seasonalComponent", name, type, as.integer(period), variance, fixed)
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_noise<-function(name, nVariance=.01, fixed=FALSE){
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "noise", name, nVariance, fixed)
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_dk_concentratedlikelihood<-function(ssf, data){
  return(.jcall("rssf/Algorithms", "D", "concentratedLikelihood", ssf@internal, as.double(data)))
}

jd3_ssf_model<-function(){
  jrslt<-.jnew("rssf/CompositeModel")
  new (Class = "JD3_SsfModel", internal = jrslt)
}

jd3_ssf_equation<-function(name, variance=.01, fixed=FALSE){
    jrslt<-.jnew("rssf/ModelEquation", name, variance, fixed)
  new (Class = "JD3_SsfEquation", internal = jrslt)
}

jd3_ssf_loading<-function(pos=NULL, weights=NULL){
  if (is.null(pos)){
    jrslt<-.jcall("demetra/ssf/implementations/Loading", "Ldemetra/ssf/ISsfLoading;", "fromPosition", as.integer(0))
    return (new (Class = "JD3_SsfLoading", internal =jrslt))
  }
  else if (length(pos) == 1){
     if (is.null(weights))
      jrslt<-.jcall("demetra/ssf/implementations/Loading", "Ldemetra/ssf/ISsfLoading;", "fromPosition", as.integer(pos))
    else{
      if (length(pos) != length(weights))
        return (NULL)
      jrslt<-.jcall("demetra/ssf/implementations/Loading", "Ldemetra/ssf/ISsfLoading;", "from", as.integer(pos), weights[1])
    }
    return (new (Class = "JD3_SsfLoading", internal =jrslt))
  }else{
    if (is.null(weights))
      jrslt<-.jcall("demetra/ssf/implementations/Loading", "Ldemetra/ssf/ISsfLoading;", "fromPositions", as.integer(pos))
    else{
    if (length(pos) != length(weights))
      return (NULL)
    jrslt<-.jcall("demetra/ssf/implementations/Loading", "Ldemetra/ssf/ISsfLoading;", "from", as.integer(pos), weights)
    }
    return (new (Class = "JD3_SsfLoading", internal =jrslt))
  }
}


