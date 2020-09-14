#' Function to extract seasonal factors and extend them using a
#' a simple forecast. Used in seasonal_overall function.
#'
#' @param df
#' @param horizon Future horizon over which to extend series, such as "5 years"
#'
#' @return
#' @export
#'
#' @examples
factor_extend <- function(df, horizon){

  # for troubleshooting
  #df <- work_m_2$result[[1]]
  #horizon <- "5 years"

  result_2 <- df %>%
    select(date, sf)

  fcast_1 <- result_2 %>%
    dplyr::mutate(date=yearmonth(date)) %>%
    as_tsibble(index=date) %>%
    fabletools::model(snaive = SNAIVE(sf ~ lag("year"))) %>%
    fabletools::forecast(h = horizon) %>%
    fabletools::as_tsibble() %>%
    # added a step to use the mean estimate as the sf estimate
    select(-sf) %>%
    rename(sf=.mean) %>%
    select(date, sf) %>%
    dplyr::mutate(date = as.Date(date))

  result_3 <- bind_rows(result_2, fcast_1) %>%
    select(date, sf)

}
