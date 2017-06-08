#' Red List Palette.
#'
#' \code{redListPalette} creates a character vector of colours suggested by the
#' IUCN for listing the 8 categories of threat
#' @return Returns a character vector of the colours used for each category of
#'   threat by the IUCN
#' @author Nicholas Murray \email{murr.nick@@gmail.com}
#' @examples
#' pie(rep(1,8), col=redListPalette(), main = "Red List Palette")
#' n <- colorRampPalette(redListPalette())(50)
#' pie(rep(1,50), col=n, frame = F)

redListPalette <- function(){
  co <- rgb (0,0,0,1)
  cr <- rgb (255/255,0,0,1)
  en <- rgb (255/255,165/255,0,1)
  vu <- rgb (255/255,255/255,0,1)
  nt <- rgb (173/255,255/255,47/255,1)
  lc <- rgb (0,128/255,0,1)
  dd <- rgb (128/255,128/255,128/255,1)
  ne <- rgb (255/255,255/255,255/255,1)
  palette <- c(co, cr, en, vu, nt, lc, dd, ne)
  return (palette)
}
