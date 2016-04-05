assessRLE <- function (inRast, fishnet){
  # assess against EOO and AOO and output a dataframe of results
  if (any(class(inRast) == 'asc')) 
    stop ('ecosystem data must be a RasterLayer')
  eco.area <- getArea(inRast)
  grain <- getCellWidth(inRast)
  eoo <- makeEOO(inRast) 
  eoo.area.km2 <- getAreaEOO(eoo)
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
  fishnet.res <- resampleFishnet(inRast, fishnet)
  aoo.1pc <- getAOO(inRast,fishnet.res, one.percent.rule = TRUE) 
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
  out.df <- data.frame (eco.area.km2 = eco.area,
                        grain = grain, 
                        eoo.area.km2 = eoo.area.km2,
                        eoo.status = eoo.status,
                        aoo.1pc = aoo.1pc,
                        aoo.status = aoo.status, 
                        overall.status = overall.status)
  return (out.df)
}