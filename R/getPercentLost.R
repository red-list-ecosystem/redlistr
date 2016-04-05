#' Percent change
#'
#' \code{getPercentLost} reports the percentage change between two numbers (such
#' as areas)
#'
#' @param A.t1 Area at t1
#' @param A.t2 Area at t2
#' @return Returns the percentage change  between these two areas
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{getAreaChangefromRast}}
#' \code{\link{getAreaChangefromShp}} \code{\link{getAreaChange}}
#' @examples
#' getAreaChange(100,80)

getPercentLost <- function(A.t1, A.t2){
  pc.lost <- ((A.t1 - A.t2)/A.t1)*100
  return (pc.lost)
}
