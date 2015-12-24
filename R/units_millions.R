
#' units_millions
#'
#' converts units to millions, taking a column of a dataframe as input
#'
#'
#' @param col
#'
#' @return
#' @export
#'
#' @examples
units_millions <- function(col) {
  col/1000000
}
