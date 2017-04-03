
######################################################################
#                                                                    #
# Congo Red Listing                                                  #
# March 2017                                                         #
#                                                                    #
# ------------------------------------------------------------------ #
#                                                                    #
# Description:                                                       #
# Testing the AOO/EOO thresholds within the Red List of Ecosystems   #
#                                                                    #
# ------------------------------------------------------------------ #
#                                                                    #
#                                                                    #
# 1. Change input file path to folder where each ecosystem is a      #
#   binary raster                                                    #
#                                                                    #
######################################################################





install.packages(c("raster", "maptools", "rgdal", "stringr"))
library(raster)
library(maptools)
library(rgdal)
library(stringr)

# Set options
options(scipen = 5)
grid.size = 10000 # size of AOO grid
saveShps = TRUE

# data
setwd("E:\\Test")
rasterOptions(tmpdir = getwd())
removeTmpFiles(0) # delete temp .gri and .grd files in 0 hour
InputDir <- "C:\\Dropbox\\Projects\\Congo\\Data\\s1_Raw\\forest_grids"
OutDir = getwd()
InputList <- list.files(InputDir, pattern = ".tif$")


# set up data capture
results.df <- data.frame (
    in.raster = NA,
    class.name = NA,
    eco.class = NA,
    grid.size = NA,
    eco.area.km2 = NA,
    eco.grain = NA,
    eoo.area.km2 = NA,
    eoo.status = NA,
    aoo.occ = NA,
    aoo.1pc = NA,
    aoo.status = NA,
    overall.status = NA,
    start.time = NA)



for (i in 1:length(InputList)){
  message (paste("working on number... ", i, " of ", length(InputList)))
  start.time = Sys.time()
  filename  = InputList[i]
  filenameshort = str_sub(filename, 0, -5)
  InputDataString <- paste(InputDir, "\\", InputList[i], sep="")
  rast = raster(InputDataString)
  NAvalue(rast) <- 0
  #values(rast)[values(rast) == 0] <- NA
  eco.area.km2 <- getArea(rast)
  message (paste("... area of ecosystem is", eco.area.km2, "km^2"))
  eco.grain <- getCellWidth(rast)
  eoo.shp = makeEOO(rast)
  eoo.area.km2 = getAreaEOO(eoo.shp)
  message (paste("... area of EOO is", eoo.area.km2, "km^2"))
  occ.no = getAOO(rast,  10000, FALSE)
  message (paste("... number of occupied grid cells is", occ.no, "10 x 10-km cells"))
  aoo.1pc = getAOO(rast,  10000, TRUE)
  message (paste("... number of AOO 1% grid cells is", aoo.1pc, "10 x 10-km cells"))

  #assess criteria
  if (eoo.area.km2 == 0){ # if 0 it means no ecosystem exists
    eoo.status <- "CO"
  } else if (eoo.area.km2 > 0 & eoo.area.km2 <= 2000){
    eoo.status <- "CR"
  } else if (eoo.area.km2 > 2000 & eoo.area.km2 <= 20000){
    eoo.status <- "EN"
  } else if (eoo.area.km2 > 20000 & eoo.area.km2 <= 50000){
    eoo.status <- "VU"
  } else {
    eoo.status <- "LC"
  }

  # aoo
  if (aoo.1pc <= 2){  # if 0 cells then it is CR, rather than CO due to the 1pc rule
    aoo.status <- "CR"
  } else if (aoo.1pc > 2 & aoo.1pc <= 20){
    aoo.status <- "EN"
  } else if (aoo.1pc > 20 & aoo.1pc <= 50){
    aoo.status <- "VU"
  } else {
    aoo.status <- "LC"
  }

  combo.status <- paste0(aoo.status, eoo.status)

  if (str_detect(combo.status,"CO") == TRUE) {
    overall.status = "CO"
  } else if (str_detect(combo.status,"CR") == TRUE) {
    overall.status = "CR"
  } else if (str_detect(combo.status,"EN") == TRUE) {
    overall.status = "EN"
  } else if (str_detect(combo.status,"VU") == TRUE) {
    overall.status = "VU"
  } else {
    overall.status = "LC"
  }
  message("... overall status of ecosystem is ", overall.status)

  ecoclass.df <- data.frame (
                        in.raster = filename,
                        class.name = filenameshort,
                        eco.class = i,
                        grid.size = grid.size,
                        eco.area.km2 = eco.area.km2,
                        eco.grain = eco.grain,
                        eoo.area.km2 = eoo.area.km2,
                        eoo.status = eoo.status,
                        aoo.occ = occ.no,
                        aoo.1pc = aoo.1pc,
                        aoo.status = aoo.status,
                        overall.status = overall.status,
                        start.time = start.time )

  if (saveShps == TRUE){
    shapefile(eoo.shp, paste0(filenameshort,"eoo"), overwrite=TRUE)
    occ.shp <- makeAOOGrid (rast, grid.size, one.percent.rule = FALSE)
    shapefile(occ.shp, paste0(filenameshort,"occ"), overwrite=TRUE)
    aoo.shp <- makeAOOGrid (rast, grid.size, one.percent.rule = TRUE)
    shapefile(aoo.shp, paste0(filenameshort,"aoo"), overwrite=TRUE)
  }

  results.df <- rbind(results.df, ecoclass.df)
  rm (ecoclass.df,rast, i, eco.area.km2, eco.grain, eoo.area.km2,
      eoo.status, occ.no, aoo.1pc, aoo.status, overall.status, start.time)
  do.call(file.remove,list(list.files(pattern=".gr*")))  # explicitly delete temp files
}

message ("Analysis complete.")
## Writing results to scratch
write.csv(results.df,
          file=paste("redlistrAnalysis", ".csv", sep = ""),
          quote=F, row.names=F)

