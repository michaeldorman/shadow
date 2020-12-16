library(sp)
library(raster)

# build
load("input/build.RData")
crs(build) = NA
usethis::use_data(build, overwrite = TRUE)
