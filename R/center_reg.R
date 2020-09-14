
#' Adds a centered regressor as a new column
#'
#' @param x is the name of the original dataframe
#' @param hold_var is the character vector name of the variable for which you want to calculate a centered regressor
#'
#' @return
#' @export
#'
#' @examples
center_reg <- function(x, hold_var) {
  out_var <- paste(hold_var, "cent", sep="_")

  # hold a copy of the original dataframe
  hold_df <- x

  hold_center <- x %>%
    mutate(year = year(date)) %>%
    mutate(month = month(date, label = FALSE))

  hold_month <- hold_center %>%
    group_by(month) %>%
    summarize_(calc_mean = interp(~ mean(var), var = as.name(hold_var)))

  out_center <- hold_center %>%
    left_join(hold_month, by = "month") %>%
    # calculates centered based on the column in hold_var minus calc_mean
    mutate_(centered = interp(~ var - calc_mean, var = as.name(hold_var))) %>%
    select(date, centered) %>%
    # renames the centered column based on contents of out_var
    rename_(.dots = setNames("centered", out_var))

  # add the newly calculated centered column to the original dataframe
  out_x <- left_join(hold_df, out_center, by = "date")
}


#' Creates a ts object out of a selected column in a data frame with a date column
#' Currently works for monthly
#'
#' @param x
#' @param var
#'
#' @return
#' @export
#'
#' @examples
create_ts_reg_m <- function(x, var) {
  out <- x %>%
    select_(.dots = c("date", var)) %>%
    read.zoo(regular=TRUE, drop=FALSE) %>%
    as.xts(order_by = "date") %>%
    convertIndex("yearmon") %>%
    # looks like dates aren’t there but they are
    as.ts(., start = start(.), end = end(.))
}
