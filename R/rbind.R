



#' rbind dataframes when you want to keep all columns
#'
#' simple function to
#' of both data frames
#' from: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
rbind.all.columns <- function(x, y) {

  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))

  x[, c(as.character(y.diff))] <- NA

  y[, c(as.character(x.diff))] <- NA

  return(rbind(x, y))
}
