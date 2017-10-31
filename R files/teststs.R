source("./R files/jd3_init.R")
source("./R files/jd3_sts.R")

load("./Data/retail.rda")
load("./Data/ABS.rda")

jd3_sts(retail$NewCarDealers)
jd3_sts(retail$BeerWineAndLiquorStores)

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:10){
  jd3_sts(ABS$X0.2.20.10.M, seasonal="Dummy" )
}

# Stop the clock
message("JD3 BSM")
proc.time() - ptm

# Start the clock!
ptm <- proc.time()

# Loop 
for (i in 1:10){
  StructTS(ABS$X0.2.20.10.M, "BSM")
}
# Stop the clock
message("R BSM")
proc.time() - ptm

plot(saDecomposition(jd3_sts(ABS$X0.2.20.10.M)))
