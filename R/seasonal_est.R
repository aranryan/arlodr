#' Estimate seasonal factors
#'
#'
#'
#' @param df Needs to have a date column, a group column that can be
# referred to by name, a var column with the variables to be
# adjusted, and a value column with the historical observations
#' @param holiday_input Optional set of monthly holiday regressors as a
#' time series
#'
#' @return
#' @export
#'
#' @examples
seasonal_est <- function(df, holiday_input="blank"){

  # for troubleshooting
  #df <- work_m_1$data[[1]]


  # create a safe version of the function so that it handles errors nicely
  safe_seas <- purrr:safely(seasonal::seas)

  # make a time series
  ts_orig <- df %>%
    tsbox::ts_ts()

  # create a variable holding the name of the first column, presumably date
  hold_name <- colnames(df)[1]

  # if there is a holiday_input that's a time series, run this first
  # approach
  if(is.ts(holiday_input)){

    df_2 <- ts_orig %>%
      safe_seas(transform.function = "log",
                xreg = hold_reg,
                forecast.maxlead = 24, # extends 24 periods ahead
                x11.appendfcst = "yes", # appends the forecast of the seasonal factors
                dir = here("output_data/hold_seasonal/"))
  } else {

    # if not, run this one (I believe it will will use default regressors)
    df_2 <- ts_orig %>%
      safe_seas(transform.function = "log",
                #xreg = hold_reg,
                forecast.maxlead = 24, # extends 24 periods ahead
                x11.appendfcst = "yes", # appends the forecast of the seasonal factors
                dir = here("output_data/hold_seasonal/"))

  }

  # from the resulting data frame extract
  result <- purrr::pluck(df_2, "result")
  # create an indicator of errors
  error <- ifelse(is.null(result), T, F)

  # check whether the error column is TRUE, indicating the
  # seasonally adjustment resulted in an error
  if(error){
    df_hold <- df %>%
      mutate(sa=value,
             sf=1)
  } else{
    # # otherwise, create a df with the adjusted data, the factors
    # # and the original data
    sa <- seasonal::final(result)
    sf <- seasonal::series(result, "d16")
    value_df <- tsbox::ts_ts(df)

    df_hold <- ts_c("value"=value_df, "sa"=sa, "sf"=sf) %>%
      tsbox::ts_tbl() %>%
      tidyr::pivot_wider(names_from=id, values_from=value) %>%
      dplyr::rename({{ hold_name }} := time)
  }

  output <- list(result=df_hold, error=error)
}
