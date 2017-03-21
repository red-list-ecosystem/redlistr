# test code

library(raster)
library(maptools)
library(microbenchmark)
InputDir <- "C:\\Dropbox\\Projects\\Congo\\Data\\s1_Raw\\forest_grids"
InputList <- list.files(InputDir, pattern = ".tif$")
i = 63 #mangrov
InputDataString <- paste(InputDir, "\\", InputList[i], sep="")
rast = raster(InputDataString)
NAvalue(rast) <- 0

microbenchmark(
AOO = getAOOShp(rast, 10000, one.percent.rule = F)
)

microbenchmark(
AOO.old = getAOO(rast, 10000, one.percent.rule = T)
)




# check on a plot
plot(grid)
plot(rast,col = "black")
plot(agg.resample, add = TRUE, col = "red")
plot(eoo.poly, add = TRUE)

# check the data
writeRaster(grid, "grid.tif", format = "GTiff")
writeRaster(x, "count.tif", format = "GTiff")
eoo.poly
