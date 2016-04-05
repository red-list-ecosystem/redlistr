getArea <- function (ecosystem.data){
  cell.res <- res(ecosystem.data)
  cell.width <- cell.res[1]
  n.cell <- ncell(ecosystem.data[values(ecosystem.data)!="NA"]) # count non NA cells
  aream2 <- (cell.width * cell.width) * n.cell
  areakm2 <- aream2/1000000
  return (areakm2)
}