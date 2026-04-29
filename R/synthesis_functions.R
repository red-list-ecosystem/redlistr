#' Compute AOO and EOO, return as table
#'
#' `bundle` performs AOO and EOO calculations on an input object and returns the results as a table
#'
#' @inheritParams getEOO
#' @param ... Additional graphical parameters passed to getAOO().
#' @return a data.frame containing AOO and EOO information for all input units as rows.
#' @author Aniko B. Toth \email{anikobtoth@@gmail.com}
#' @family synthesis functions
#' @export


bundle <-  function(input_data, names_from = NA, ...) {
  UseMethod("bundle", input_data)
}

#' @export
bundle.sf <-  function(input_data, names_from = NA, ...){
  eoo <- getEOO(input_data, names_from = names_from)
  aoo <- getAOO(input_data, names_from = names_from, ...)

  eootable <- list2table(eoo)
  aootable <- list2table(aoo)

  merge(aootable, eootable, by = c("name", "input_class"))
}

#' @export
bundle.SpatRaster <- function(input_data, ...){
  eoo <- getEOO(input_data)
  aoo <- getAOO(input_data)

  eootable <- list2table(eoo)
  aootable <- list2table(aoo)

  merge(aootable, eootable, by = c("name", "input_class"))
}


#' Summarises a list of EOO or AOOgrid objects in a table
#'
#' `list2table` wrapper that summarises a list of EOO or AOOgrid objects as a table.
#' @param l a list of EOO or AOOgrid objects
#' @return a data.frame
#' @family synthesis functions
#' @export

list2table <- function(l){
  sapply(l, as.list) |> t() |> data.frame(row.names = NULL)
}
