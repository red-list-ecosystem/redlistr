getAreaChangefromRast <- function (r1, r2){
  a1 <- getArea (r1)
  a2 <- getArea (r2)
  a.dif <- a1 - a2
  return(a.dif)
}
