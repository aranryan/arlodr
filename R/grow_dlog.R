#' take a test dataframe with predicted series of dlogs and use it to grow out the actual series of interest
#' Also see grow_dlog2.
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

#' Take a test data frame with predicted series of dlogs and use it to grow out the actual series of interest.
#' Similar to grow_dlog, but this uses data in df format, while the other uses data in xts format. Also, this
#' returns the data in df format.
#'
#' @param predict_df the object with predicted dlog values, it is just the
# future period.
#' @param train_df train_df is the object with historical data, it is used
# for the level from which to grow the first data point. Both predict_df
# and train_df have the same columns. They include a field for the dependent variable
# in dlog as well as levels.
#' @param predict_dlog character vector name of the predicted dlog series
#' @param predict_level character vector name of the level series we are creating
#'
#' @return
#' @export
#'
#' @examples
grow_dlog2 <- function (predict_df, train_df, predict_dlog, predict_level) {

  # convert data from df to xts
  predict_xts <- predict_df %>%
    data.frame() %>%
    zoo::read.zoo(regular=TRUE, drop=FALSE) %>%
    xts()

    # convert data from df to xts
  train_xts <- train_df %>%
    data.frame() %>%
    zoo::read.zoo(regular=TRUE, drop=FALSE) %>%
    xts()

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
    dplyr::select(-fcast_log)
}
