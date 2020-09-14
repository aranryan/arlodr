#' Extend the number of strdays by recalculating
#'
#' @param df Requires a "date" column
#' @param freq Either "m", "q" or "a"
#'
#' @return
#' @export
#'
#' @examples extend_strdays(freq="m")
extend_strdays <- function(df, freq){
  if(freq=="m") {
    df_1 <- df %>%
      mutate(strdays = lubridate::days_in_month(date)) %>%
      mutate(strdays = ifelse(lubridate::month(date)==2, 28, strdays))
  } else if (freq=="q") {
    df_1 <- df %>%
      mutate(strdays = case_when(lubridate::month(date)==1 ~ 90,
                                 lubridate::month(date)==4 ~ 91,
                                 lubridate::month(date)==7 ~ 92,
                                 lubridate::month(date)==10 ~92,
                                 T ~ NA_real_))
  } else if(freq=="a") {
    df_1 <- df %>%
      mutate(strdays = 365)
  } else{
    df_1 <- "error"
  }
}
