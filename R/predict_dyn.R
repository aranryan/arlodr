
#' create predictions using dyn$predict
#' created based on
#' http://stackoverflow.com/a/13098860/1770086
#' also based on the help for dyn, see example at end
#' @param model
#' @param train
#' @param test
#' @param dependentvar
#'
#' @return
#' @export
#'
#' @examples
predict_dyn <- function(model, train, test, dependentvar) {
  Ntrain <- nrow(train)
  Ntest <- nrow(test)
  testtraindata <- rbind(train, test )
  for( i in 1:Ntest ) {
    result <- tail(dyn$predict(model,newdata=testtraindata[1:Ntrain+i,]),1)
    testtraindata[Ntrain+i,dependentvar] <- result[1,1]
  }
  return(testtraindata[(Ntrain+1):(Ntrain + Ntest),] )
}




#' take a test dataframe with predicted series of dlogs and use it to grow out the actual series of interest
#'
#' @param predict_xts
#' @param train_xts
#' @param predict_dlog
#' @param predict_level
#'
#' @return
#' @export
#'
#' @examples
grow_dlog <- function (predict_xts, train_xts, predict_dlog, predict_level) {
  # predict_xts is the xts object with predicted dlog values, it is just the
  # future period train_xts is the xts object with historical data, it is used
  # for the level from which to grow the first data point.
  # predict_dlog is the character vector name of the predicted dlog series
  # predict_level is the character vector name of the level series we are creating

  # number of rows
  Npred <- nrow(predict_xts)

  hold1 <- predict_xts %>%
    # add the last row of history from the training data to the top of the predicted data
    rbind(tail(train_xts, 1), .) %>%
    data.frame(date=time(.), .) %>%
    # calculate a temporary fcast_log column based which will only have a value for the first row
    dplyr::mutate_(fcast_log = lazyeval::interp(~ log(var), var = as.name(predict_level)))
  # use a loop to grow out fcast_log from its first value row by row using predict_dlog
  for(i in 1:Npred){
    hold1[i+1, "fcast_log"] <- hold1[i,"fcast_log"] + hold1[i+1, predict_dlog]
  }
  # drop the first row, which had been brought in from the training data
  hold1 <- hold1[-c(1), ]

  # go through a series of steps to calculate the predict_level series based on fcast_log
  # setup a formula that will be an input in the mutate_ call as the right hand side of the equation
  varval <- lazyeval::interp(~exp(fcast_log))
  hold2 <- hold1 %>%
    # give mutate two inputs (Alist of functions to be applied on the input variables and a character vector of output variables names)
    dplyr::mutate_(.dots= setNames(list(varval), predict_level)) %>%
    dplyr::select(-fcast_log) %>%
    zoo::read.zoo(regular=TRUE, drop=FALSE) %>%
    xts::xts()
}


#' evaluates the test versus actual
#'
#' @param full_data
#' @param test_data
#' @param dependentvar
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples


eval_test_actual <- function(full_data, test_data, dependentvar, end_date, market_text, test_start){
  # full_data is the original xts object with training and test data
  # test_data is just the test period, includes value of the dependent variable in levels
  # dependent variable is a character vector naming the dependent variable

  # take the full data, extract the dependent variable in levels
  hold1 <- full_data %>%
    data.frame(date=time(.), .) %>%
    select_(.dots = c("date", dependentvar)) %>%
    rename_(.dots = setNames(dependentvar, "actual"))

  # take the test data and extract the dependent variable in levels
  holdtest <- test_data %>%
    data.frame(date=time(.), .) %>%
    select_(.dots = c("date", dependentvar)) %>%
    rename_(.dots = setNames(dependentvar, "predicted")) %>%
    mutate(area_sh = market_text) %>%
    mutate(variable = dependentvar) %>%
    mutate(test_start = test_start) %>%
    select(date, area_sh, variable, test_start, everything())
}

#' create the test and train data
#'
#' @param df
#' @param varlist
#' @param dates
#' @param testNA1
#' @param testNA2
#'
#' @return
#' @export
#'
#' @examples
create_test_train <- function(df, varlist, dates, testNA1, testNA2) {

  temp_full <- df %>%
    data.frame() %>%
    dplyr::select_(.dots = c("date", varlist)) %>%
    zoo::read.zoo(regular=TRUE, drop=FALSE) %>%
    window(start = dates$full_start, end = dates$full_end) %>%
    xts::xts()

  # create training data
  temp_train <- temp_full %>%
    window(start = dates$train_start, end = dates$train_end) %>%
    xts::xts()

  # create test data
  temp_test <- temp_full %>%
    window(start = dates$test_start, end = dates$test_end) %>%
    xts::xts()

  # set certain named columns of the test data to NA
  namevector <- c(testNA1,testNA2)
  temp_test[,namevector] <- NA

  out <- list("temp_full" = temp_full,
              "temp_train" = temp_train,
              "temp_test" = temp_test)
}
