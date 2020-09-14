
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

#' Create predictions using dyn$predict, setup to start with and return
#' data frames rather than xts
#'
#' In other words, in this version, you can supply data frames as arguments
#' and receive a data frame as output. This is a bit friendlier with a map
#' approach.
#'
#' created based on
#' http://stackoverflow.com/a/13098860/1770086
#' also based on the help for dyn, see example at end
#' @param model Typically model fitted with dyn$lm
#' @param train Data frame with first column in date format so that function
#' can convert to xts easily for calcs.
#' @param test Data frame with first column in date format.
#' @param dependentvar Character string naming the dependentvar that is to be
#' grown out iteratively
#'
#' @return
#' @export
#'
#' @examples
predict_dyn2 <- function(model, train, test, dependentvar) {
  # convert data from df to xts
  train <- train %>%
    data.frame() %>%
    zoo::read.zoo(regular=TRUE, drop=FALSE) %>%
    xts()

  # convert data from df to xts
  test <- test %>%
    data.frame() %>%
    zoo::read.zoo(regular=TRUE, drop=FALSE) %>%
    xts()

  Ntrain <- nrow(train)
  Ntest <- nrow(test)
  testtraindata <- rbind(train, test )
  for( i in 1:Ntest ) {
    result <- tail(dyn$predict(model,newdata=testtraindata[1:Ntrain+i,]),1)
    testtraindata[Ntrain+i,dependentvar] <- result[1,1]
  }
  testtraindata[(Ntrain+1):(Ntrain + Ntest),] %>%
    # convert from xts to dataframe
    data.frame(date=time(.), .)
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



#' Fits a model
#'
#' Simple approach to fit a model based on a data frame using the dyn package.
#' Includes a step to convert the data frame to xts before fitting.
#'
#' @param formula
#' @param data
#'
#' @return
#' @export
#'
#' @examples
fit_dyn <- function(formula, data) {
  # convert data from df to xts, which is needed for fit
  data_xts <- data %>%
    data.frame() %>%
    zoo::read.zoo(regular=TRUE, drop=FALSE) %>%
    xts()
  # fit model
  dyn$lm(formula = formula, data = data_xts)
}

