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
