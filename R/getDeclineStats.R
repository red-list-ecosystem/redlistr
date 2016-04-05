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