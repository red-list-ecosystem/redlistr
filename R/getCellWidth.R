getCellWidth <- function (ecosystem.data){
  # Returns the pixel resolution of the input dataset
  cell.res <- res(ecosystem.data)
  cellwidth <- cell.res[1]
  return(cellwidth)
}