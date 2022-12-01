# header ------------------------------------------------------------------
# run all


# run modules -------------------------------------------------------------

# duration: approx. 8 minutes

time0 <- Sys.time()

source("10_birth-fertility.r")
source("11_birth-rel.r")
source("20_death.r")
source("30_immigration.r")
source("40_emigration.r")
source("50_conversion.r")
source("60_demography-popscen.r")
source("70_area.r")

time1 <- Sys.time()
time1 - time0
