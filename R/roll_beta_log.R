#' Estimates trend growth on quarterly data
#'
#' For use in roll_beta_log
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
log_growth_q <- function(x) {
  print(tail(x))
  mod <- try(lm(x[,"logvalue"] ~ time(x[,"date"])))
  # if the model worked
  if (!inherits(mod, "try-error")) {
    # extract the slope coefficient estimate from the second row
    res <- broom::tidy(mod)[2,"estimate"]
    # annualize from quarterly
    res <- ((1+res)^4 -1)
  } else {
    # if the model didn't work
    print("error")
    res <- NA
  }
}

#' Calculates rolling regression estimates
#'
#' Uses rollapply to estimate equation results.
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
roll_beta_log <- function (x) {
  df1 <- x %>%
    # create a log value column
    mutate(logvalue = log(value)) %>%
    mutate(trendint = 1:nrow(.)) %>%
    # in the following, the "[" is the selection operator, which opens
    # up a selection that is then used to grab three columns, which
    # are fed into rollapply, which then returns a single value that
    # goes into the beta column
    mutate(beta = rollapply(
      .[, c("date", "value", "logvalue")],
      FUN = log_growth_q,
      width = 41,
      fill = NA,
      by.column = FALSE,
      align = "right",
      partial = 30
    )) %>%
    select(-logvalue,-trendint)
}


#' Estimate annualized growth
#'
#' See example_growth_trend in vignettes for usage. This makes an estimate even
#' if there are only a couple actual data points.
#'
#' @param df
#' @param periods_year the number of periods in a year, e.g. 4 for quarters or
#' 365.25 for days. This will convert the slope coefficient to an annualized growth
#' rate by doing ((1 + estimate) ^ periods_year - 1)
#'
#' @return
#' @export
#'
#' @examples
log_growth_1 <- function(df, periods_year) {
  # check if all rows are na
  if(all(is.na(df[,"y"]))){
    # if they are all NA, then put NA in res and return it
    res <- NA
  } else{
    mod <- try(lm(df[, "y"] ~ df[, "x"]))
    if (!inherits(mod, "try-error")) {
      res <- broom::tidy(mod)[2, "estimate"]
      res <- ((1 + res) ^ periods_year - 1)
    } else {
      #print("error")
      res <- NA
    }
  }}
