source("./R files/jd3_init.R")
source("./R files/jd3_fractionalairline.R")
source("./R files/jd3_x11.R")
source("./R files/jd3_stl.R")
source("./R files/jd3_holidays.R")

usclaims<-read.table("./Data/usclaims.txt")

w<-jd3_periodicAirline(usclaims[,2], periods=365.25/7, outliers=c("ao", "ls", "wo"), criticalValue = 6)

print(dictionary(w))
print(result(w,"outliers"))

edf<-read.table("./Data/edf.txt")
y<-log(edf[,1])

jhol<-jd3_holidays()
add(jhol, "NewYear")
add(jhol, "MayDay")
add(jhol, "Assumption")
add(jhol, "AllSaintsDay")
add(jhol, "Christmas", offset=-1)
add(jhol, "Christmas")
add(jhol, "EasterMonday")
add(jhol, "Ascension")
add(jhol, "WhitMonday")
add(jhol, c(7, 14))

hol<-jd3_holidaysMatrix(jhol, "1996-01-01", length = length(y), type = "SkipSundays")
d<-jd3_periodicAirline(y, x=hol, periods=c(7, 365.25), outliers=c("ao", "ls", "wo"), criticalValue = 6)

print(result(d,"parameters"))
print(result(d,"outliers"))
print(result(d, "b"))
print(result(d, "t"))
y<-result(d, "lin")

c<-jd3_fractionalAirlineDecomposition(y, period=7, TRUE)
c1<-jd3_fractionalAirlineDecomposition(result(c,"sa"), period=365.25, adjust = FALSE)
# The final decomposition is given by
w<-result(c,"s")
t<-result(c1,"t")
sa<-result(c1,"sa")
s<-result(c1,"s")
i<-result(c1,"i")
seatsdecomp<-cbind(y,t,sa,w,s,i)

y<-exp(y)
# sa of daily series with X11
a<-jd3_x11(y, period=7, multiplicative = TRUE, seas0="S3X15", seas1="S3X15", trendLength = 9)
a1<-jd3_x11(result(a,"d11"), period=365.25, multiplicative = TRUE, trendLength = 367)

# The final decomposition is given by
w<-result(a,"d10")
t<-result(a1,"d12")
sa<-result(a1,"d11")
s<-result(a1,"d10")
i<-result(a1,"d13")
x11decomp<-cbind(y,t,sa,w, s,i)

b<-jd3_stl(y, period=7, multiplicative = TRUE, swindow=15, twindow=9)
b1<-jd3_stl(result(b,"sa"), period=365, multiplicative = TRUE)

# The final decomposition is given by
w<-result(b,"s")
t<-result(b1,"t")
sa<-result(b1,"sa")
s<-result(b1,"s")
i<-result(b1,"i")
stldecomp<-cbind(y,t,sa,w,s,i)

