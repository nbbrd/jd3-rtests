source("./R files/jd3_init.R")
source("./R files/jd3_sts.R")
source("./R files/jd3_ssf.R")

load("./Data/retail.rda")
load("./Data/ABS.rda")

jd3_sts(retail$NewCarDealers)
jd3_sts(retail$BeerWineAndLiquorStores)

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:100){
  jd3_sts(ABS$X0.2.20.10.M, seasonal="Crude" )
}


# Stop the clock
message("JD3 BSM")
print(proc.time() - ptm)

ptm <- proc.time()
# Loop 
for (i in 1:100){
  bsm<-jd3_ssf_model()

  # create the components and add them to the model
  ssf.add(bsm, jd3_ssf_locallineartrend("ll"))
  ssf.add(bsm, jd3_ssf_seasonal("s", 12, type="Crude"))
  # create the equation (fix the variance to 1)
  eq<-jd3_ssf_equation("eq", 1, TRUE)
  ssf.add(eq, "ll")
  ssf.add(eq, "s")
  ssf.add(bsm, eq)
  #estimate the model
  rslt<-ssf.estimate(bsm, ABS$X0.2.20.10.M, marginal=FALSE, concentrated=TRUE)
}


# Stop the clock
message("JD3 BSM2")
print(proc.time() - ptm)

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:10){
  StructTS(ABS$X0.2.20.10.M, "BSM")
}
# Stop the clock
message("R BSM")
print(proc.time() - ptm)

plot(saDecomposition(jd3_sts(ABS$X0.2.20.10.M)))

