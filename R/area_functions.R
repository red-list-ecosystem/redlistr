getCellWidth <- function (ecosystem.data){
  # Returns the pixel resolution of the input dataset
  cell.res <- res(ecosystem.data)
  cellwidth <- cell.res[1]
  return(cellwidth)
}

getArea <- function (ecosystem.data){
  cell.res <- res(ecosystem.data)
  cell.width <- cell.res[1]
  n.cell <- ncell(ecosystem.data[values(ecosystem.data)!="NA"]) # count non NA cells
  aream2 <- (cell.width * cell.width) * n.cell
  areakm2 <- aream2/1000000
  return (areakm2)
}

getAreaChangefromRast <- function (r1, r2){
  a1 <- getArea (r1)
  a2 <- getArea (r2)
  a.dif <- a1 - a2
  return(a.dif)
}

getAreaChangefromShp <- function(p1, p2){
  a1 <- gArea(p1)
  a2 <- gArea(p2)
  a.dif.m2 <- a1 - a2
  a.dif.km2 <- a.dif.m2/1000000
  return (a.dif.km2)
}

difRast <- function (r1, r2){
  p1 <- rasterToPolygons(r1, dissolve = T)
  p2 <- rasterToPolygons(r2, dissolve = T)
  dif.p <- gDifference(p1, p2)
  dif.r <- rasterize(dif.p, r1)
  return (dif.r)
}

# Criterion A stuff
getARD <- function (A.t1, A.t2, year.t1, year.t2){
  # Absolute Rate of Change (also known as Annual Change(R)) in Puyrvaud
  ARD <- (A.t2-A.t1)/(year.t2-year.t1)
  ARD <- -ARD # make it a positive number to be consistend with Keith et al 2009
  return (ARD)
}

getPRD <- function (A.t1, A.t2, year.t1, year.t2){
  # Proportional rate of change (also known as trajectory (r))
  PRD <- 100 * (1-(A.t2/A.t1)^(1/(year.t2-year.t1)))
  return (PRD)
}

getARC <- function (A.t1, A.t2, year.t1, year.t2){
  # Annual rate of change from Puyravaud 2004. Also known as instantaneous rate of change.
  ARC <- (1/(year.t2-year.t1))*log(A.t2/A.t1)
  return (ARC)
}

getPRD2 <- function (A.t1, A.t2, year.t1, year.t2){
  PRD2 <- 100 * (1-(exp(log(A.t2/A.t1)/(year.t2-year.t1)))) 
  return (PRD2)
}


getAreaChange <- function (A.t1, A.t2){
  area.change <- A.t1 - A.t2
  return(area.change)
}

getPercentLost <- function(A.t1, A.t2){
  pc.lost <- ((A.t1 - A.t2)/A.t1)*100
  return (pc.lost)
}

futureAreaEstimate <- function(A.t1, year.t1, PRD, ARD, ARC, nYears = 50){
  A.PRD.t3 <- A.t1 * (1 -(PRD/100))^nYears
  A.ARC.t3 <- A.t1 * (1 + ARC)^nYears
  A.ARD.t3 <- A.t1 - (ARD*nYears)
  if (A.PRD.t3 < 0) A.PRD.t3 = 0
  if (A.ARC.t3 < 0) A.ARC.t3 = 0
  if (A.ARD.t3 < 0) A.ARD.t3 = 0
  y.t3 <- year.t1+nYears
  pc.change.prd <- getPercentLost (A.t1,A.PRD.t3)
  pc.change.arc <- getPercentLost (A.t1,A.ARC.t3) 
  pc.change.ard <- getPercentLost (A.t1,A.ARD.t3)
  out <- data.frame(area.t1 = A.t1, 
                    year.t1 = year.t1,
                    prop.rate.decl = PRD,
                    abs.rate.decl = ARD, 
                    annual.rate.change = ARC,
                    forecast.year = y.t3,
                    forecast.area.prd = A.PRD.t3,
                    forecast.area.arc = A.ARC.t3,
                    forecast.area.ard = A.ARD.t3,
                    pc.change.prd = pc.change.prd,
                    pc.change.arc = pc.change.arc,
                    pc.change.ard = pc.change.ard)
  return(out)
}

getDeclineStats <- function (A.t1, A.t2, year.t1, year.t2){
  # consider raw code rather than a function in here
  ARD <- getARD (A.t1, A.t2, year.t1, year.t2)
  PRD <- getPRD (A.t1, A.t2, year.t1, year.t2)
  ARC <- getARC (A.t1, A.t2, year.t1, year.t2)
  area.change <- getAreaChange (A.t1, A.t2)
  pc.lost <- getPercentLost (A.t1, A.t2)
  out <- data.frame(area.t1 = A.t1,
                    area.t2 = A.t2,
                    year.t1 = year.t1,
                    year.t2 = year.t2,
                    abs.rate.decl = ARD, 
                    prop.rate.decl = PRD,
                    annual.rate.change = ARC,
                    area.change = area.change,
                    percent.lost = pc.lost)
  return (out)
}
