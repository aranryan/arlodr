



#' estimate slope coefficient on quarterly data, using log format
#'
#' function to
#' df is a dataframe that is in tidy format with the variables in columns
#'   and a date column.
#' gp_vec variable that is used in the group_by to estimate the slope for
#'   different metro areas
#' y1 is the dependent variable
#' x1 is the time trend
#' transf_l is the label to be used in the transf column
#' originally wrote this as part of the fred "work1_bk.R" script
#'
#'
#'# In writing the function above I had started with the following simple example
#' 20 year empetot regression
#' regdf <- tmbk_q %>%
#'   filter(date >= start_20yr & date <= end_prior_q) %>%
#'   group_by(area_sh) %>%
#'   do(fiteq = lm((log(empetot)) ~ time(date), data = .))
#'
#'   get the coefficients by group in a tidy data_frame
#' dfslope <- tidy(regdf, fiteq) %>%
#'   filter(term == "time(date)") %>%
#'   mutate(value = (((1+estimate)^4) -1)) %>%
#'   mutate(transf = "gr20yr") %>%
#'   mutate(variable = "empetot") %>%
#'   select(area_sh, variable, transf, value)
#'
#' get the summary statistics by group in a tidy data_frame
#' dfsumm = glance(regdf, fiteq)
#'
#' @param df
#' @param start_date
#' @param end_date
#' @param gp_vec
#' @param y1
#' @param x1
#' @param transf_l
#'
#' @return
#' @export
#'
#' @examples
slope_log_q_1=function(df, start_date, end_date, gp_vec, y1, x1, transf_l){
  # creates a filter criteria to use to drop nas in the y1 variable
  filter_criteria <- paste0("!is.na(",interp(y1), ")")
  regdf <- df %>%
    filter(date >= start_date & date <= end_date) %>%
    # following seems to work, wasn't sure it would
    group_by_(gp_vec) %>%
    filter_(filter_criteria) %>%
    # based on
    # http://stackoverflow.com/questions/26657938/how-to-make-lm-interpret-eval-in-formula
    do(fiteq = lm(as.formula(paste0("log(", y1, ") ~ ", x1, sep="")), data = .))
  # get the coefficients by group in a tidy data_frame
  dfslope <- broom::tidy(regdf, fiteq) %>%
    filter(term == "time(date)") %>%
    # annualizes the slope coefficient estimate
    mutate(value = (((1+estimate)^4) -1)) %>%
    # painful to get here, based on NSE vignette to use variable
    mutate(transf = lazyeval::interp(transf_l)) %>%
    mutate(variable = lazyeval::interp(y1)) %>%
    select(area_sh, variable, transf, value)
  # get the summary statistics by group in a tidy data_frame
  dfsumm <- broom::glance(regdf, fiteq)
  out_x <- list(dfslope=dfslope, dfsumm=dfsumm)
  return(out_x)
}



