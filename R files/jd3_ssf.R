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
  Class="JD3_SsfMeasurement",
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

setMethod("estimate", signature = c(object="JD3_SsfModel"), function(object, data, precision=1e-15, marginal=FALSE, concentrated=TRUE, initialParameters=NULL){
  if ( is.jnull(object@internal) ){
    return(NULL)
  }else{
    jparams<-.jnull("[D")
    if (! is.null(initialParameters))
      jparams<-.jarray(initialParameters)
    jdata<-matrix_r2jd(data)
    jrslt<-.jcall(object@internal, "Lrssf/CompositeModel$Estimation;", "estimate", jdata, precision, marginal, concentrated, jparams)
    return( new(Class= "JD3_ProcResults", internal=jrslt))
  }
})

setMethod("compute", signature = c(object="JD3_SsfModel"), function(object, data, parameters, marginal=FALSE, concentrated=TRUE){
  if ( is.jnull(object@internal) ){
    return(NULL)
  }else{
    jdata<-matrix_r2jd(data)
    jrslt<-.jcall(object@internal, "Lrssf/CompositeModel$Estimation;", "compute", jdata, .jarray(parameters), marginal, concentrated)
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

jd3_ssf_noise<-function(name, variance=.01, fixed=FALSE){
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "noise", name, variance, fixed)
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

jd3_ssf_loading_sum<-function(length=0){
  if (length == 0)
    jrslt<-.jcall("demetra/ssf/implementations/Loading", "Ldemetra/ssf/ISsfLoading;", "sum")
  else
    jrslt<-.jcall("demetra/ssf/implementations/Loading", "Ldemetra/ssf/ISsfLoading;", "createPartialSum", as.integer(length))
  return (new (Class = "JD3_SsfLoading", internal =jrslt))
}

jd3_ssf_loading_cyclical<-function(period, startpos){
  jrslt<-.jcall("demetra/ssf/implementations/Loading", "Ldemetra/ssf/ISsfLoading;", "cyclical", as.integer(period), as.integer(startpos-1))
  return (new (Class = "JD3_SsfLoading", internal =jrslt))
}

jd3_ssf<-function(initialization, dynamics, measurement){
  jrslt<-.jcall("rssf/Ssf", "Ldemetra/ssf/univariate/Issf;", "of", initialization@internal, dynamics@internal, measurement@internal)
  new (Class = "JD3_Ssf", internal = jrslt)
}

jd3_ssf_arima<-function(ar, diff, ma, var=1){
  jrslt<-.jcall("rssf/AtomicModels", "Ldemetra/ssf/univariate/ISsf;", "arima", as.double(ar), as.double(diff), as.double(ma), var)
  new (Class = "JD3_Ssf", internal = jrslt)
}

jd3_ssf_arma<-function(ar, ma, var=1){
  jrslt<-.jcall("rssf/AtomicModels", "Ldemetra/ssf/univariate/ISsf;", "arma", as.double(ar), as.double(ma), var)
  new (Class = "JD3_Ssf", internal = jrslt)
}

jd3_ssf_sarima<-function(period, orders, seasonal, parameters){
  jrslt<-.jcall("rssf/AtomicModels", "Ldemetra/ssf/univariate/ISsf;", "sarima", as.integer(period), as.integer(orders), as.integer(seasonal), parameters)
  new (Class = "JD3_Ssf", internal = jrslt)
}

jd3_ssf_reg<-function(ssf, x, var=0, mvar=NULL){
  
  if (is.null(mvar)){
    if (var == 0)
      jssf<-.jcall("rssf/RegressionModels", "Ldemetra/ssf/univariate/ISsf;", "fixed", ssf@internal, matrix_r2jd(x))
    else
      jssf<-.jcall("rssf/RegressionModels", "Ldemetra/ssf/univariate/ISsf;", "timeVarying", ssf@internal, matrix_r2jd(x), var)
  }else{
    jssf<-.jcall("rssf/RegressionModels", "Ldemetra/ssf/univariate/ISsf;", "timeVarying", ssf@internal, matrix_r2jd(x), matrix_r2jd(mvar))
  }
  return (new (Class = "JD3_Ssf", internal = jssf))
}

jd3_ssf_td<-function(name, period, start, length, groups=c(1,2,3,4,5,6,0), contrast=TRUE, variance, fixed=FALSE){
  jdomain<-tsdomain_r2jd(period, startYear = start[1], startPeriod = start[2], length = length)
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "tdRegression", name, jdomain, as.integer(groups), contrast, variance, fixed)
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_rawtd<-function(name, period, start, length, groups=c(1,2,3,4,5,6,0), variances, fixed=FALSE){
  jdomain<-tsdomain_r2jd(period, startYear = start[1], startPeriod = start[2], length = length)
  jrslt<-.jcall("rssf/AtomicModels", "Lrssf/ModelItem;", "rawTdRegression", name, jdomain, as.integer(groups), .jarray(variances), fixed)
  new (Class = "JD3_SsfItem", internal = jrslt)
}

jd3_ssf_initialization<-function(stationaryVar, initialState=NULL, diffuseConstraints=NULL, Pi=NULL){
  jp<-matrix_r2jd(stationaryVar)
  if (is.null(diffuseConstraints) && is.null(Pi)){
    if (is.null(initialState))
      jstate=.jnull("[D")
    else
      jstate=.jarray(initialState)
    jinit<-.jcall("rssf/Initialization", "Ldemetra/ssf/ISsfInitialization;", "of", jstate, jp)
  }
  else{
    if (is.null(initialState))
      jstate=.jnull("[D")
    else
      jstate=.jarray(initialState)
    jb<-matrix_r2jd(diffuseConstraints)
    jpi<-matrix_r2jd(Pi)
    jinit<-.jcall("rssf/Initialization", "Ldemetra/ssf/ISsfInitialization;", "ofDiffuse", jstate, jp, jb, jpi)
  }
  return (new (Class = "JD3_SsfInitialization", internal = jinit))
}
