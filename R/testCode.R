# test code
library(raster)
library(maptools)
library(microbenchmark)
library(rgdal)
library(stringr)

# setup up testing
devtools::use_testthat()

# make a raster
library(raster)
r <- raster(ncol=100, nrow=100)
r[] <- 1:1000
plot(r)




# ##
# # data
setwd("C:\\_NickMurray\\test")
r1 <- readAsciiGrid("force10test_spatial_coral.txt")
r1 <- raster(r1)
plot(r1)
r1
isLonLat(r1) # true --> need to change to a projected coordinate system
getAOO(r1, 10000, TRUE)
getAOO(r1, 10000, FALSE)

# InputDir <- "C:\\Dropbox\\Projects\\Congo\\Data\\s1_Raw\\forest_grids"
# OutDir = getwd()
# InputList <- list.files(InputDir, pattern = ".tif$")
# i = 72 #mangrov
# InputDataString <- paste(InputDir, "\\", InputList[i], sep="")
# ecosystem.data = x = rast = raster(InputDataString)
# NAvalue(ecosystem.data) <- NAvalue(x) <- NAvalue(rast) <- 0
#
# # test the Grid
# grid.size = 10000
# grid = createGrid(rast, grid.size)
# grid #87000 10x10km cells
# getCellWidth(grid) #10000 metres cell
#
# # save it as a shapefile
# proj = crs(rast)
# grid.shp <- rasterToPolygons(grid, dissolve=TRUE)
# grid.shp
#
# # check  area
# # tested in ArcGIS on shapefile: 4023.346668km2, 402334.66678ha, 4023346667.8m2
# # tested with RLETools.py: Area of raster dataset: 4043.0 km2
# area = getArea(rast)
# area #4043
#
# # check resolution
# getCellWidth(rast) #250 is correct
#
# # Test AOO
# AOO = getAOOShp(rast, 10000, one.percent.rule = F)
# AOO.old = getAOO(rast, 10000, one.percent.rule = T)
# makeAOOGrid
#
# # check on a plot
# plot(grid)
# plot(rast,col = "black")
# plot(agg.resample, add = TRUE, col = "red")
# plot(eoo.poly, add = TRUE)
#
# # check the data
# writeRaster(grid, "grid.tif", format = "GTiff")
# writeRaster(x, "count.tif", format = "GTiff")
#
#
# # set up private key on github
# cat("GITHUB_PAT=1b73ebb7a505d0559d75fa7341d5ff400964f486\n",
#     file=file.path(normalizePath("~/"), ".Renviron"),
#     append=TRUE)
#
#
# ## Test on projected  Remap data
# ecosystem.data = x = rast = raster("remap3_project.tif")
# proj4string(x) #
# plot(x)
# z = makeEOO(rast)
# getAreaEOO(z) # ArcGIS reports 1712.13023km2
# shapefile(z, "eooremap", overwrite=TRUE)
# plot(z, add = T)
#
# options(scipen = 5)
#
# # test on geographic data from REMPA
# ecosystem.data = x = rast = raster("remapseive.tif")
# ecosystem.data # def don't want more than one value. Need a presence absence map.
# a = makeEOO(rast2)
# getAreaEOO(a) # crazy number
# isLonLat(rast) # true --> need to change to a projected coordinate system
# sr <- "+proj=utm +zone=54 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"  # reproject to UTM
# rast <- projectRaster(rast, crs = sr,   method = "ngb") # need to use ngb rather than default (bilinear) so nearest neighbour is used
# rast2
# plot(rast2)
# rast3 <- rast2 == 2
# plot (rast3)
# a = makeEOO(rast3)
# getAreaEOO(a)
# b = getAOO(rast3,  10000, TRUE)
# b
#
# ########### FULL FINAL
# # run over several classes
#
# rasterOptions(tmpdir = getwd())
# removeTmpFiles(1) # delete temp .gri and .grd files in 1 hour
# ecosystem.data = raster("C:\\Dropbox\\Projects\\Congo\\Data\\s1_Raw\\forest_grids\\ext11320_250.tif")
# NAvalue(ecosystem.data) <- 0
# filename  = filename(ecosystem.data)
# isLonLat(ecosystem.data) # true --> need to change to a projected coordinate system
#
# #sr <- "+proj=utm +zone=54 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"  # reproject to UTM
# #inRast <- projectRaster(ecosystem.data, crs = sr,   method = "ngb") # use nearest neigbour (ngb) to preserve class values rather than default (bilinear)
# inRast = ecosystem.data
# grid.size = 10000
# plot(inRast)
#
#
# # set up data capture
# results.df <- data.frame (
#     in.raster = NA,
#     eco.class = NA,
#     grid.size = NA,
#     eco.area.km2 = NA,
#     eco.grain = NA,
#     eoo.area.km2 = NA,
#     eoo.status = NA,
#     aoo.occ = NA,
#     aoo.1pc = NA,
#     aoo.status = NA,
#     overall.status = NA,
#     start.time = NA)
#
#
# # start on raster
# vals = freq(inRast, useNA = "no") # get class values from raster
# valList = vals[,1] # convert list of values
# message('Raster has >>> ', length(valList) , ' <<< classes' )
#
# for (i in valList){
#   message ("working on class... ", i)
#   start.time = Sys.time()
#   rast <- inRast == i
#   values(rast)[values(rast) == 0] <- NA
#   eco.area.km2 <- getArea(rast)
#   message (paste("... area of ecosystem is", eco.area.km2, "km^2"))
#   eco.grain <- getCellWidth(rast)
#   eoo.shp = makeEOO(rast)
#   eoo.area.km2 = getAreaEOO(eoo.shp)
#   message (paste("... area of EOO is", eoo.area.km2, "km^2"))
#   occ.no = getAOO(rast,  10000, FALSE)
#   message (paste("... number of occupied grid cells is", occ.no, "10 x 10-km cells"))
#   aoo.1pc = getAOO(rast,  10000, TRUE)
#   message (paste("... number of AOO 1% grid cells is", aoo.1pc, "10 x 10-km cells"))
#
#   #assess criteria
#   if (eoo.area.km2 == 0){ # if 0 it means no ecosystem exists
#     eoo.status <- "CO"
#   } else if (eoo.area.km2 > 0 & eoo.area.km2 <= 2000){
#     eoo.status <- "CR"
#   } else if (eoo.area.km2 > 2000 & eoo.area.km2 <= 20000){
#     eoo.status <- "EN"
#   } else if (eoo.area.km2 > 20000 & eoo.area.km2 <= 50000){
#     eoo.status <- "VU"
#   } else {
#     eoo.status <- "LC"
#   }
#
#   # aoo
#   if (aoo.1pc <= 2){  # if 0 cells then it is CR, rather than CO due to the 1pc rule
#     aoo.status <- "CR"
#   } else if (aoo.1pc > 2 & aoo.1pc <= 20){
#     aoo.status <- "EN"
#   } else if (aoo.1pc > 20 & aoo.1pc <= 50){
#     aoo.status <- "VU"
#   } else {
#     aoo.status <- "LC"
#   }
#
#   combo.status <- paste0(aoo.status, eoo.status)
#
#   if (str_detect(combo.status,"CO") == TRUE) {
#     overall.status = "CO"
#   } else if (str_detect(combo.status,"CR") == TRUE) {
#     overall.status = "CR"
#   } else if (str_detect(combo.status,"EN") == TRUE) {
#     overall.status = "EN"
#   } else if (str_detect(combo.status,"VU") == TRUE) {
#     overall.status = "VU"
#   } else {
#     overall.status = "LC"
#   }
#
#   ecoclass.df <- data.frame (
#                         in.raster = filename,
#                         eco.class = i,
#                         grid.size = grid.size,
#                         eco.area.km2 = eco.area.km2,
#                         eco.grain = eco.grain,
#                         eoo.area.km2 = eoo.area.km2,
#                         eoo.status = eoo.status,
#                         aoo.occ = occ.no,
#                         aoo.1pc = aoo.1pc,
#                         aoo.status = aoo.status,
#                         overall.status = overall.status,
#                         start.time = start.time )
#
#   results.df <- rbind(results.df, ecoclass.df)
#   rm (ecoclass.df,rast, i, eco.area.km2, eco.grain, eoo.area.km2,
#       eoo.status, occ.no, aoo.1pc, aoo.status, overall.status, start.time)
# }
#
# message ("Analysis complete.")
# ## Writing results to scratch
# write.csv(results.df,
#           file=paste("redlistrAnalysis", ".csv", sep = ""),
#           quote=F, row.names=F)
#
