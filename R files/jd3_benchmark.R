source("./R files/jd3_ts.R")

jd3_denton<-function(s, t, d=1, mul=TRUE, modified=TRUE, conversion="Sum"){
  jd_s<-ts_r2jd(s)
  jd_t<-ts_r2jd(t)
  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/timeseries/TsData;", "denton"
                  ,jd_s, jd_t, as.integer(d), mul, modified, conversion)
  ts_jd2r(jd_rslt)
}

jd3_cholette<-function(s, t, rho=1, lambda=1, bias="None", conversion="Sum"){
  jd_s<-ts_r2jd(s)
  jd_t<-ts_r2jd(t)
  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/timeseries/TsData;", "cholette"
                  ,jd_s, jd_t, rho, lambda, bias, conversion)
  ts_jd2r(jd_rslt)
}

jd3_mcholette<-function(xlist, tcvector=NULL, ccvector=NULL, rho=1, lambda=1) {
  if(!is.list(xlist) | length(xlist)<3 ) {
    stop("incorrect argument, first argument should be a list of at least 3 time series")}
  
  #create the input
  jdic=.jnew("demetra.util.r.Dictionary")
  for(i in seq_along(xlist)){
    .jcall(jdic, "V", "add", names(xlist[i]), ts_r2jd(xlist[[i]]))
  }
  if (is.null(tcvector)){
    ntc=0
    jtc<-.jcast(.jnull(), "[Ljava/lang/String;")
  }else if (! is.vector(tcvector)){
    stop("incorrect argument, constraints should be presented within a character vector")
  }else{
    ntc<-length(tcvector)
    jtc<-.jarray(tcvector, "java/lang/String")
  }
  if (is.null(ccvector)){
    ncc=0
    jcc<-.jcast(.jnull(), "[Ljava/lang/String;")
  }else if (! is.vector(ccvector)){
    stop("incorrect argument, constraints should be presented within a character vector")
  }else{
    ncc<-length(ccvector)
    jcc<-.jarray(ccvector, "java/lang/String")
  }
  if(ntc+ncc==0) {
    stop("both constraint types are empty, include at least one temporal or contemporaneous constraint")}
  
  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/util/r/Dictionary;", "multiCholette"
                  ,jdic,  jtc, jcc, rho, lambda)
  if (is.jnull(jd_rslt))
    return (NULL)
  rlist=list()
  rnames=.jcall(jd_rslt, "[S", "names")
  for(i in seq_along(rnames)){
    jts<-.jcall(jd_rslt, "Ldemetra/timeseries/TsData;", "get", rnames[i])
    if (! is.jnull(jts)){
      rlist[[rnames[i]]]<-ts_jd2r(jts)
    }
  }
  return (rlist)
}

