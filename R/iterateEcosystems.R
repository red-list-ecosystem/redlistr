# test code
install.packages(c("raster", "maptools", "rgdal", "stringr"))
library(raster)
library(maptools)
library(rgdal)
library(stringr)

rasterOptions(tmpdir = getwd())
removeTmpFiles(1) # delete temp .gri and .grd files in 1 hour

# data
setwd("C:\\_NickMurray\\test")
InputDir <- "C:\\Dropbox\\Projects\\Congo\\Data\\s1_Raw\\forest_grids"
OutDir = getwd()

# single multiclass ecosystem raster
ecosystem.data = raster("remapseive.tif")
NAvalue(ecosystem.data) <- 0
filename  = filename(ecosystem.data)
isLonLat(ecosystem.data) # if true --> need to change to a projected coordinate system
sr <- "+proj=utm +zone=54 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"  # reproject to UTM
ecosystem.data <- projectRaster(ecosystem.data, crs = sr,   method = "ngb") # use nearest neigbour (ngb) to preserve class values rather than default (bilinear)
inRast = ecosystem.data
grid.size = 10000

# set up data capture
results.df <- data.frame (
  in.raster = NA,
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


# start on the multiclass raster
vals = freq(inRast, useNA = "no") # get class values from raster
valList = vals[,1] # convert list of values
message('Raster has >>> ', length(valList) , ' <<< classes' )

for (i in valList){
  message ("working on class... ", i)
  start.time = Sys.time()
  rast <- inRast == i
  values(rast)[values(rast) == 0] <- NA
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

  ecoclass.df <- data.frame (
    in.raster = filename,
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

  results.df <- rbind(results.df, ecoclass.df)
  rm (ecoclass.df,rast, i, eco.area.km2, eco.grain, eoo.area.km2,
      eoo.status, occ.no, aoo.1pc, aoo.status, overall.status, start.time)
}

message ("Analysis complete.")
## Writing results to scratch
write.csv(results.df,
          file=paste("redlistrAnalysis", ".csv", sep = ""),
          quote=F, row.names=F)

