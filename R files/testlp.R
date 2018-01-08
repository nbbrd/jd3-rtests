source("./R files/jd3_x11.R")
source("./R files/jd3_stl.R")

load("./Data/retail.rda")
load("./Data/ABS.rda")

lp_endpoints<-function(s, horizon, k="Henderson", ic=4.5){
  l<-length(s)
  q<-2*horizon
  x11<-jd3_x11(s, period = frequency(s))
  sa<-result(x11, "d11")
  sac<-sa[1:(l-horizon)]
  D = 4.0 / (pi * ic * ic)
  daf<-jd3_localpolynomials(sac, horizon, kernel=k)
  lc<-jd3_localpolynomials(sac, horizon, kernel=k, endpoints="LC", ic = D)
  ql<-jd3_localpolynomials(sac, horizon, kernel=k, endpoints="QL", ic= D)
  cq<-jd3_localpolynomials(sac, horizon, kernel=k, endpoints="CQ", ic=D)
  h<-jd3_henderson(sac, horizon*2+1)
  H<-jd3_henderson(sa, horizon*2+1)
  plot(cq[(l-2*q):(l-horizon)], type="l")
  lines(daf[(l-2*q):(l-horizon)],col="red")
  lines(lc[(l-2*q):(l-horizon)],col="blue")
  lines(ql[(l-2*q):(l-horizon)],col="green")
  lines(h[(l-2*q):(l-horizon)],col="gray")
  lines(H[(l-2*q):(l-horizon)],col="magenta")
  cbind(h,daf, lc,ql,cq)[(l-2*q):(l-horizon),]
}