


#' Combined steps of seasonal adjustment
#'
#' @param df should be a data frame that contains a date column, a group column, a var column, and a value column. var is the column that contains the variable names, such as var.
#' @param group grouping column, such as geoseg. Entered without quotes.
#' @param holiday_input
#' @param horizon
#'
#' @return
#' @export
#'
#' @examples output_m <- seasonal_overall(work_m, group=geoseg,
#' holiday_input=hold_reg, horizon="5 years")

seasonal_overall <- function(df, group, holiday_input="blank", horizon="5 years"){

  work_1 <- df %>%
    group_by({{ group }}, var) %>%
    tidyr::nest()

  work_2 <- work_1 %>%
    # estimates seasonal factors
    dplyr::mutate(output = map(data, .f=seasonal_est, holiday_input=holiday_input)) %>%
    mutate(result = map(output, pluck("result"))) %>%
    mutate(error = map(output, pluck("error"))) %>%
    select(-output) %>%
    # extracts factors and extends to future using a naive forecast
    # that repeats values
    mutate(factor = map(result, factor_extend, horizon=horizon))

  # extracts data frame of seasonal factors
  factors <- work_2 %>%
    select({{ group }}, var, factor) %>%
    unnest(cols=c(factor)) %>%
    rename(value=sf) %>%
    mutate(var = paste0(var, "sf")) %>%
    ungroup() %>%
    select(date, {{ group }}, var, everything())

  # extracts data frame showing any series that were an
  # error. For those the seasonal factor was set to 1.
  errors <- work_2 %>%
    select({{ group }}, var, error) %>%
    ungroup()

  output <- list(factors=factors, errors=errors)
}
