#' Fit a model after filtering data frame to particular geography and traning period.
#'
#' @param area_sh_use
#' @param train_start_date
#' @param train_end_date
#' @param spec
#' @param data
#'
#' @return
#' @export
#'
#' @examples
fit_model_dyn <- function(area_sh_use, train_start_date, train_end_date, spec, data){
  # filter data to area_sh
  data_1 <- data %>%
    filter(area_sh == area_sh_use) %>%
    select(-area_sh)
  # make training data
  train_data <- filter_datew(data_1, train_start_date, train_end_date)
  # fit model. The possibly wrapper gives the NA to use as the error if it doesn't fit.
  model <- fit_dyn(formula = as.formula(spec), data = train_data)
}

#' Title
#'
#' @param area_sh_use
#' @param train_start_date
#' @param train_end_date
#' @param spec
#' @param data
#'
#' @return
#' @export
#'
#' @examples Helpful to do as follows, putting possibly wrapper, with NA,
#' around the call on the function.
#'
#' new_1 <- mkeq_use_3 %>%
#' mutate(
#'  model = pmap(.l=list(area_sh, train_start_date,
#'                       train_end_date, spec),
#'               .f=possibly(arlodr::fit_model_dyn_c, NA), data = df_use1)
#')
#'
fit_model_dyn_c <- function(area_sh_use, train_start_date, train_end_date, spec, data){
  # filter data to area_sh
  data_1 <- data %>%
    filter(area_sh == area_sh_use) %>%
    select(-area_sh)
  # make training data
  train_data <- filter_datew(data_1, train_start_date, train_end_date)
  # Fit model
  model <- fit_dyn(formula = as.formula(spec), data = train_data)
}


#' Make predictions based on growing out dlogs, after filtering to particular geography.
#'
#' @param area_sh_use
#' @param train_start_date
#' @param train_end_date
#' @param test_start_date
#' @param test_end_date
#' @param model
#' @param dependentvar
#' @param depend_in_levels
#' @param data
#'
#' @return
#' @export
#'
#' @examples
ar_add_predictions <- function(area_sh_use, train_start_date, train_end_date,
                            test_start_date, test_end_date, model, dependentvar,
                            depend_in_levels, data) {
  # filter data to area_sh
  data_1 <- data %>%
    filter(area_sh == area_sh_use) %>%
    select(-area_sh)
  # make train data
  train_data <- filter_datew(data_1, train_start_date, train_end_date)
  # make test data
  test_data <- filter_datew(data_1, test_start_date, test_end_date)
  predictions <- arlodr::predict_dyn2(model, train=train_data, test=test_data, dependentvar=dependentvar)
  prediction_levels <- grow_dlog2(predictions, train_data, dependentvar, depend_in_levels)
}

#' Pull out the predictation data column.
#'
#' @param pred_lev_data
#' @param depvar_lev_chr
#'
#' @return
#' @export
#'
#' @examples
extract_pred_lev <- function(pred_lev_data, depvar_lev_chr){
  # pred_lev_data is just the test period, includes predicted value of the
  # dependent variable in levels. This just extracts that using the name
  # of the series to provide a bit of confirmation

  # take the prediction data and extract the dependent variable in levels
  holddata <- pred_lev_data %>%
    # select by name of column
    select_(.dots = c("date", depvar_lev_chr)) %>%
    # alternative approach I considered was to select date and second column
    # select(date, 2) %>%
    gather(predvar, predicted, -date) %>%
    select(date, predvar, everything())
}
