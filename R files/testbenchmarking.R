source("./R files/jd3_init.R")
source("./R files/jd3_benchmark.R")

load("./Data/retail.rda")

y<-jd3_aggregate(retail$RetailSalesTotal, 1)

denton_mul<-jd3_denton(retail$RetailSalesTotalExclMotorVehicle, y)
denton_add<-jd3_denton(retail$RetailSalesTotalExclMotorVehicle, y, mul = FALSE)

cholette_mul<-jd3_cholette(retail$RetailSalesTotalExclMotorVehicle, y)
cholette_add<-jd3_cholette(retail$RetailSalesTotalExclMotorVehicle, y, rho=1, lambda = 0)

print(summary(denton_mul-cholette_mul))
print(summary(denton_add-cholette_add))
