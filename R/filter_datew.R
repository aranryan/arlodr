


#' Filter a data frame based on a date column and
#' specific start and end dates.
#'
#' @param df
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
filter_datew <- function(df, start_date, end_date){df %>%
    filter(date >= start_date & date <= end_date)
}
