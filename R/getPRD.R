
getPRD <- function (A.t1, A.t2, year.t1, year.t2){
  # Proportional rate of change (also known as trajectory (r))
  PRD <- 100 * (1-(A.t2/A.t1)^(1/(year.t2-year.t1)))
  return (PRD)
}


# getPRD2 <- function (A.t1, A.t2, year.t1, year.t2){
#   PRD2 <- 100 * (1-(exp(log(A.t2/A.t1)/(year.t2-year.t1)))) 
#   return (PRD2)
# }
