getAreaChangefromShp <- function(p1, p2){
  a1 <- gArea(p1)
  a2 <- gArea(p2)
  a.dif.m2 <- a1 - a2
  a.dif.km2 <- a.dif.m2/1000000
  return (a.dif.km2)
}