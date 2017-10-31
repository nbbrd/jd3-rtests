source("./R files/jd3_init.R")
source("./R files/jd3_airlinedecomposition.R")

load("./Data/retail.rda")
load("./Data/ABS.rda")

jd3_airlineDecomposition(retail$NewCarDealers )
jd3_airlineDecomposition(retail$BeerWineAndLiquorStores)

plot(saDecomposition(jd3_airlineDecomposition(ABS$X0.2.20.10.M)))