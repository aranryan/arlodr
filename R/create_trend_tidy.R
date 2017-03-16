#' Append a trend variable
#'
#' Based on a date column, append a trend variable to a data frame
#'
#' @param df Data frame to be modified
#' @param date_var Column containing the date (enter as character vector)
#' @param frequency Number of period per year (enter as number)
#'
#' @return
#' @export
#'
#' @examples
#'   create_trend(frequency=4, date_var = "date")
#'
create_trend_tidy <- function(df,date_var,frequency) {
  df %>%
    mutate_(interval = lazyeval::interp(
      ~lubridate::interval(as.Date(lubridate::origin), var),
      var = as.name(date_var))) %>%
    mutate(period = as.period(interval, unit="months")) %>%
    mutate(trend = as.numeric(period, "years")*frequency) %>%
    select(-interval, -period)
}
