source("./R files/jd3_init.R")
source("./R files/jd3_fractionalairline.R")

usclaims<-read.table("./Data/usclaims.txt")

w<-jd3_periodicAirline(usclaims[,2], periods=365.25/7, outliers=c("ao", "ls", "wo"), criticalValue = 5)

print(dictionary(w))
print(result(w,"outliers"))

births<-read.table("./Data/births.txt")
d<-jd3_periodicAirline(births[,1], periods=c(7, 365.25), outliers=c("ao", "wo"), criticalValue = 6)

print(result(d,"parameters"))
print(result(d,"outliers"))
