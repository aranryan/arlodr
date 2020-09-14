
#' Compounded Annual Rate of Change
#' (Ending Value / Initial Value) ^ ( 1 / # of periods) - 1)
#'
#' @param x
#' @param n
#'
#' @return
#' @export
#'
#' @examples
calc_cagr <- function(x, n){
  (x/lag(x,n))^(1/n)-1}

#' Compounded Annual Rate of Change (not sure the difference)
#' (((x(t)/x(t-1)) ** (n_obs_per_yr)) - 1) * 100
#'
#' @param x
#' @param p
#'
#' @return
#' @export
#'
#' @examples
calc_annualized <- function(x, p) {
  (x/lag(x,1))^(p)-1}
