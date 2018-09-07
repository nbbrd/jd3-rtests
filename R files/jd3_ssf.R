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

setMethod("estimate", signature = c(object="JD3_SsfModel"), function(object, data, marginal=FALSE){
  if ( is.jnull(object@internal) ){
    return(NULL)
  }else{
    jdata<-matrix_r2jd(data)
    jrslt<-.jcall(object@internal, "Lrssf/CompositeModel$Estimation;", "estimate", jdata, marginal)
    return( new(Class= "JD3_ProcResults", internal=jrslt))
  }
})

setMethod("add", signature = c(object="JD3_SsfEquation", item="character"), function(object, item, coeff=NULL, loading=NULL){
  if ( is.jnull(object@internal))
    return
  if (is.null(coeff)){
    if (is.null(loading))
      .jcall(object@internal, "V", "add", item)
    else
      .jcall(object@internal, "V", "add", item, .jnull("java/lang/Double"), loading@internal)
  }
  else{
  if (is.null(loading))
    .jcall(object@internal, "V", "add", item, .jnew("java/lang/Double", coeff),.jnull("demetra/ssf/ISsfLoading"))
  else
    .jcall(object@internal, "V", "add", item, .jnew("java/lang/Double", coeff), loading@internal)
  }
})

jd3_ssf_ar<-function(name, nar, ar=NULL, variance=-1, nlags=0, fixed=FALSE){
  jar=ar
  if (is.null(jar)){
    jar=.jnull("[D")
  }
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "ar", name, as.integer(nar), jar, variance, as.integer(nlags), fixed)
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_locallevel<-function(name, levelVariance=-1){
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "localLevel", name, levelVariance)
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_locallineartrend<-function(name, levelVariance=-1, slopevariance=-1){
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "localLinearTrend", name, levelVariance, slopevariance)
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_seasonal<-function(name, period, type="Crude", seasVariance=-1){
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "seasonalComponent", name, type, as.integer(period), seasVariance)
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_noise<-function(name, nVariance=-1){
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "noise", name, nVariance)
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_dk_concentratedlikelihood<-function(ssf, data){
  return(.jcall("rssf/Algorithms", "D", "concentratedLikelihood", ssf@internal, as.double(data)))
}

jd3_ssf_model<-function(){
  jrslt<-.jnew("rssf/CompositeModel")
  new (Class = "JD3_SsfModel", internal = jrslt)
}

jd3_ssf_equation<-function(name, variance=-1){
  if (variance <0)
    jrslt<-.jcall("rssf/ModelEquation", "Lrssf/ModelEquation;", "withError", name)
  else
    jrslt<-.jcall("rssf/ModelEquation", "Lrssf/ModelEquation;", "withFixedError", name, variance)
  new (Class = "JD3_SsfEquation", internal = jrslt)
}

jd3_ssf_loading<-function(pos){
  if (is.null(pos)){
    jrslt<-.jcall("demetra/ssf/implementations/Loading", "Ldemetra/ssf/ISsfLoading;", "fromPosition", as.integer(0))
    return (new (Class = "JD3_SsfLoading", internal =jrslt))
  }
  else if (length(pos) == 1){
    jrslt<-.jcall("demetra/ssf/implementations/Loading", "Ldemetra/ssf/ISsfLoading;", "fromPosition", as.integer(pos))
    return (new (Class = "JD3_SsfLoading", internal =jrslt))
  }else{
    jrslt<-.jcall("demetra/ssf/implementations/Loading", "Ldemetra/ssf/ISsfLoading;", "fromPositions", as.integer(pos))
    return (new (Class = "JD3_SsfLoading", internal =jrslt))
  }
}


