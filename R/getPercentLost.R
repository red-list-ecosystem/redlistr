getPercentLost <- function(A.t1, A.t2){
  pc.lost <- ((A.t1 - A.t2)/A.t1)*100
  return (pc.lost)
}