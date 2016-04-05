# Criterion A stuff
getARD <- function (A.t1, A.t2, year.t1, year.t2){
  # Absolute Rate of Change (also known as Annual Change(R)) in Puyrvaud
  ARD <- (A.t2-A.t1)/(year.t2-year.t1)
  ARD <- -ARD # make it a positive number to be consistend with Keith et al 2009
  return (ARD)
}