source("./R files/jd3_x11.R")
source("./R files/jd3_stl.R")

load("./Data/retail.rda")
load("./Data/ABS.rda")

lp_endpoints<-function(s, horizon, kernel="Henderson", ic=4.5){
  l<-length(s)
  q<-2*horizon
  x11<-jd3_x11(s, period = frequency(s))
  sa<-result(x11, "d11")
  sac<-sa[1:(l-horizon)]
  daf<-jd3_localpolynomials(sac, horizon, kernel=kernel, endpoints="DAF")
  cc<-jd3_localpolynomials(sac, horizon, kernel=kernel, endpoints="CC", ic=ic)
  lc<-jd3_localpolynomials(sac, horizon, kernel=kernel, endpoints="LC", ic=ic)
  ql<-jd3_localpolynomials(sac, horizon, kernel=kernel, endpoints="QL", ic=ic)
  cq<-jd3_localpolynomials(sac, horizon, kernel=kernel, endpoints="CQ", ic=ic)
  h<-jd3_henderson(sac, horizon*2+1, ic=ic)
  H<-jd3_henderson(sa, horizon*2+1)
  plot(cq[(l-2*q):(l-horizon)], type="l")
  lines(daf[(l-2*q):(l-horizon)],col="red")
  lines(cc[(l-2*q):(l-horizon)],col="brown")
  lines(lc[(l-2*q):(l-horizon)],col="blue")
  lines(ql[(l-2*q):(l-horizon)],col="green")
  lines(h[(l-2*q):(l-horizon)],col="gray")
  lines(H[(l-2*q):(l-horizon)],col="magenta")
  cbind(h,daf, lc,ql,cq)[(l-2*q):(l-horizon),]
}

lp_endpoints(ABS$X0.2.09.10.M, horizon = 11)
