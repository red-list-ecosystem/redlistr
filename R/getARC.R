getARC <- function (A.t1, A.t2, year.t1, year.t2){
  # Annual rate of change from Puyravaud 2004. Also known as instantaneous rate of change.
  ARC <- (1/(year.t2-year.t1))*log(A.t2/A.t1)
  return (ARC)
}