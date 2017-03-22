# test code
library(raster)
library(maptools)
library(microbenchmark)
library(rgdal)

# data
setwd("C:\\_NickMurray\\test")
InputDir <- "C:\\Dropbox\\Projects\\Congo\\Data\\s1_Raw\\forest_grids"
OutDir = getwd()
InputList <- list.files(InputDir, pattern = ".tif$")
i = 63 #mangrov
InputDataString <- paste(InputDir, "\\", InputList[i], sep="")
ecosystem.data = x = rast = raster(InputDataString)
NAvalue(ecosystem.data) <- NAvalue(x) <- NAvalue(rast) <- 0

# test the Grid
grid.size = 10000
grid = createGrid(rast, grid.size)
grid #87000 10x10km cells
getCellWidth(grid) #10000 metres cell

# save it as a shapefile
proj = crs(rast)
grid.shp <- rasterToPolygons(grid, dissolve=TRUE)
grid.shp

# check  area
# tested in ArcGIS on shapefile: 4023.346668km2, 402334.66678ha, 4023346667.8m2
# tested with RLETools.py: Area of raster dataset: 4043.0 km2
area = getArea(rast)
area #4043

# check resolution
getCellWidth(rast) #250 is correct

# Test AOO
AOO = getAOOShp(rast, 10000, one.percent.rule = F)
AOO.old = getAOO(rast, 10000, one.percent.rule = T)
makeAOOGrid

# check on a plot
plot(grid)
plot(rast,col = "black")
plot(agg.resample, add = TRUE, col = "red")
plot(eoo.poly, add = TRUE)

# check the data
writeRaster(grid, "grid.tif", format = "GTiff")
writeRaster(x, "count.tif", format = "GTiff")


# set up private key on github
cat("GITHUB_PAT=1b73ebb7a505d0559d75fa7341d5ff400964f486\n",
    file=file.path(normalizePath("~/"), ".Renviron"),
    append=TRUE)


## Test on Remap data
ecosystem.data = x = rast = raster("remap3_project.tif")
proj4string(x) #
plot(x)
z = makeEOO(rast)
getAreaEOO(z) # ArcGIS reports 1712.13023km2
shapefile(z, "eooremap", overwrite=TRUE)
plot(z, add = T)


