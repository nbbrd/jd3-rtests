source("./R files/jd3_init.R")
source("./R files/jd3_arima.R")

load("./Data/retail.rda")

jd3_arima(retail$NewCarDealers , order=c(3,1,0), seasonal=c(0,1,1) )
arima(retail$NewCarDealers , order=c(3,1,0), seasonal=c(0,1,1), optim.control = list(reltol=1e-9) )


jd3_arima(retail$NewCarDealers , order=c(3,1,1), seasonal=c(0,1,1) )
arima(retail$NewCarDealers , order=c(3,1,1), seasonal=c(0,1,1), optim.control = list(reltol=1e-9) )


jd3_arima(retail$NewCarDealers , order=c(3,1,0), seasonal=c(0,1,1) )
arima(retail$NewCarDealers , order=c(3,1,0), seasonal=c(0,1,1), optim.control = list(reltol=1e-9) )


jd3_arima(retail$BeerWineAndLiquorStores , order=c(2,1,1), seasonal=c(0,1,1) )
arima(retail$BeerWineAndLiquorStores , order=c(2,1,1), seasonal=c(0,1,1), optim.control = list(reltol=1e-9) )

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:20){
  jd3_arima(retail$BeerWineAndLiquorStores , order=c(3,1,1), seasonal=c(0,1,1) )
}

# Stop the clock
message("JD3 ([3 1 1][0 1 1])")
proc.time() - ptm

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:20){
  arima(retail$BeerWineAndLiquorStores , order=c(3,1,1), seasonal=c(0,1,1), optim.control = list(reltol=1e-9) )
}

# Stop the clock
message("R-Arima ([3 1 1][0 1 1])")
proc.time() - ptm

ptm <- proc.time()
# Loop 
for (i in 1:20){
  jd3_arima(retail$BeerWineAndLiquorStores , order=c(0,1,1), seasonal=c(0,1,1) )
}

# Stop the clock
message("JD3 (airline)")
proc.time() - ptm

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:20){
  arima(retail$BeerWineAndLiquorStores , order=c(0,1,1), seasonal=c(0,1,1), optim.control = list(reltol=1e-9) )
}

# Stop the clock
message("R-Arima (airline)")
proc.time() - ptm

a<-retail$BookStores
a[10]<-NaN
a[100]<-NaN

ptm <- proc.time()
# Loop 
for (i in 1:100){
  jd3_arima(a , order=c(0,1,1), seasonal=c(0,1,1) )
}

# Stop the clock
message("JD3 (airline) with missings")
proc.time() - ptm

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:100){
  arima(a , order=c(0,1,1), seasonal=c(0,1,1), optim.control = list(reltol=1e-9) )
}

# Stop the clock
message("R-Arima (airline) with missings")
proc.time() - ptm

logLik( jd3_arima(a , order=c(0,1,1), seasonal=c(0,1,1) ))
coef( jd3_arima(a , order=c(3,1,1), seasonal=c(1,1,0) ))
logLik( arima(a , order=c(0,1,1), seasonal=c(0,1,1), kappa=1e9, optim.control = list(reltol=1e-12) ))
coef( arima(a , order=c(3,1,1), seasonal=c(1,1,0), kappa=1e9, optim.control = list(reltol=1e-12) ))

preda<-predict(jd3_arima(a , order=c(0,1,1), seasonal=c(1,1,0) ), nforecasts=12, method="all")
result(preda, "forecasts")
result(preda, "forecasts.se")

predb<-predict(arima(a , order=c(0,1,1), seasonal=c(1,1,0) ), 12)
show(predb$pred)
show(predb$se)

