source("./R files/jd3_init.R")
source("./R files/jd3_procresults.R")
source("./R files/jd3_arimapredict.R")

setClass(
  Class="JD3_Ssf",
  contains = "JD3_ProcResults"
)

setClass(
  Class="JD3_SsfDynamics",
  contains = "JD3_ProcResults"
)

setClass(
  Class="JD3_SsfInitialization",
  contains = "JD3_ProcResults"
)

setClass(
  Class="JD3_SsfMeasurement",
  contains = "JD3_ProcResults"
)

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
  jrslt<-.jcall("rssf/AtomicModels", "Ldemetra/ssf/univariate/ISsf;", "sarima", as.integer(period), as.integer(orders), as.integer(seasonal), .jarray(parameters))
  new (Class = "JD3_Ssf", internal = jrslt)
}

jd3_ssf_locallevel<-function(levelVariance, noiseVariance){
  jrslt<-.jcall("rssf/AtomicModels", "Ldemetra/ssf/univariate/ISsf;", "localLevel", levelVariance, noiseVariance)
  new (Class = "JD3_Ssf", internal = jrslt)
}

jd3_ssf_locallineartrend<-function(levelVariance, slopeVariance, noiseVariance){
  jrslt<-.jcall("rssf/AtomicModels", "Ldemetra/ssf/univariate/ISsf;", "localLinearTrend", levelVariance, slopeVariance, noiseVariance)
  new (Class = "JD3_Ssf", internal = jrslt)
}

jd3_ssf_seasonal<-function(period, seasVariance, type="Crude"){
  jrslt<-.jcall("rssf/AtomicModels", "Ldemetra/ssf/univariate/ISsf;", "seasonalComponent", as.integer(period), type, seasVariance)
  new (Class = "JD3_Ssf", internal = jrslt)
}

jd3_ssf_cycle<-function(dumpingFactor, cyclicalPeriod, cvar){
  jrslt<-.jcall("rssf/AtomicModels", "Ldemetra/ssf/univariate/ISsf;", "cycle", dumpingFactor, cyclicalPeriod, cvar)
  new (Class = "JD3_Ssf", internal = jrslt)
}

jd3_ssf_dk_concentratedlikelihood<-function(ssf, data){
  return(.jcall("rssf/Algorithms", "D", "concentratedLikelihood", ssf@internal, as.double(data)))
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