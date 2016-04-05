#' Area Change.
#'
#' \code{getAreaChange} reports the difference two araes.
#'
#' @param A.t1 Area at t1
#' @param A.t2 Area at t2
#' @return Returns the difference between these two areas
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @seealso \code{\link{getAreaChangefromRast}}
#' \code{\link{getAreaChangefromShp}}
#' @examples
#' getAreaChange(100,80)

getAreaChange <- function (A.t1, A.t2){
  area.change <- A.t1 - A.t2
  return(area.change)
}
