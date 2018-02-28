source("./R files/jd3_init.R")
source("./R files/jd3_fractionalairline.R")
source("./R files/jd3_x11.R")
source("./R files/jd3_stl.R")
source("./R files/jd3_tests.R")

births<-read.table("./Data/births.txt")
deaths<-read.table("./Data/deaths.txt")
edf<-read.table("./Data/edf.txt")
y<-edf[,1]

# sa of daily series with X11
a<-jd3_x11(y, period=7, multiplicative = FALSE, seas0="S3X15", seas1="S3X15", trendLength = 21)
a1<-jd3_x11(result(a,"d11"), period=365.25, multiplicative = FALSE, trendLength = 367)
wa1<-result(a1, "d10")

# The final decomposition is given by
w<-result(a,"d10")
t<-result(a1,"d12")
sa<-result(a1,"d11")
s<-result(a1,"d10")
i<-result(a1,"d13")
x11decomp<-cbind(y,t,sa,w,s,i)
##

plot(wa1[2000:2500], type="l")

a2<-jd3_x11(result(a,"d11"), period=365, multiplicative = FALSE, trendLength = 367)
wa2<-result(a2, "d10")
plot(wa2[2000:2500], type="l")

b<-jd3_stl(y, period=7, multiplicative = FALSE, swindow=15, twindow=75)
b1<-jd3_stl(result(b,"sa"), period=365, multiplicative = FALSE)
wb<-result(b1, "s")
plot(wb[2000:2500], type="l")

# The final decomposition is given by
w<-result(b,"s")
t<-result(b1,"t")
sa<-result(b1,"sa")
s<-result(b1,"s")
i<-result(b1,"i")
stldecomp<-cbind(y,t,sa,w,s,i)

c<-jd3_fractionalAirlineDecomposition(y, period=7, adjust = FALSE)
c1<-jd3_fractionalAirlineDecomposition(result(c,"sa"), period=365.25, adjust = TRUE)
wc<-result(c1, "s")
plot(wc[2000:2500], type="l")

# The final decomposition is given by
w<-result(c,"s")
t<-result(c1,"t")
sa<-result(c1,"sa")
s<-result(c1,"s")
i<-result(c1,"i")
seatsdecomp<-cbind(y,t,sa,w,s,i)
