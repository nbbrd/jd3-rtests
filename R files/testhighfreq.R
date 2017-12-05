source("./R files/jd3_init.R")
source("./R files/jd3_fractionalairline.R")
source("./R files/jd3_x11.R")
source("./R files/jd3_stl.R")

births<-read.table("./Data/births.txt")

a<-jd3_x11(births[,1], period=7, multiplicative = FALSE, seas0="S3X15", seas1="S3X15", henderson = 367)
a1<-jd3_x11(result(a,"d11"), period=365.25, multiplicative = FALSE, henderson = 367)
wa1<-result(a1, "d10")
plot(wa1[2000:2500], type="l")

a2<-jd3_x11(result(a,"d11"), period=365, multiplicative = FALSE, henderson = 367)
wa2<-result(a2, "d10")
plot(wa2[2000:2500], type="l")

b<-jd3_stl(births[,1], period=7, multiplicative = FALSE, swindow=15)
b<-jd3_stl(result(b,"sa"), period=365, multiplicative = FALSE)
wb<-result(b, "s")
plot(wb[2000:2500], type="l")

c<-jd3_fractionalAirlineDecomposition(births[,1], period=7, adjust = FALSE)
c<-jd3_fractionalAirlineDecomposition(result(c,"sa"), period=365.25, adjust = TRUE)
wc<-result(c, "s")
plot(wc[2000:2500], type="l")
