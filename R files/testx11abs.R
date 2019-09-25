source("./R files/jd3_init.R")
source("./R files/jd3_x11.R")
source("./R files/jd3_fractionalairline.R")

DM2013to2018<-read.table("./Data/DM2013to2018.txt", header=TRUE, sep='\t',stringsAsFactors=FALSE)
str(DM2013to2018) #Inspect data structure)

y<-DM2013to2018[,4]

###############################################################################
#plot(y, type='l')
#tsdisplay(y)
#NSW_DMSales<-ts(DM2013to2018Year[,4], frequency = 365.25/7, start=2013-1/365.25) 
#mstl(taylor) %>% autoplot(facet=TRUE)
#mstl(NSW_DMSales, lambda='auto') %>% autoplot(facet=TRUE)
###############################################################################

m1<-jd3_x11(y, 365.25/7, trend.horizon=26, seas.s0="S3X1",seas.s1="S3X3")
fsa1<-saDecomposition(m1)
m2<-jd3_x11(y, 52, trend.horizon=26, seas.s0="S3X1",seas.s1="S3X3")


fsa2<-saDecomposition(m2)
n<-dim(fsa1)[1]
idx<-(n-53):n
plot(idx, fsa1[idx,4], "l")
lines(idx, fsa2[idx,4], col="red")

a1<-jd3_fractionalAirlineDecomposition(log(y), 365.25/7)
fsa3<-saDecomposition(a1)
lines(idx, exp(fsa3[idx,4]), col="blue")


plot(1:dim(fsa1)[1], fsa1[,1], "l", col="gray")
lines(fsa2[,"trend"], col="red")
lines(fsa1[,"trend"], col="blue")
lines(exp(fsa3[,"trend"]), col="black")

#plot(jd3_henderson(result(m1, "d11bis"), length=53), type="l")

