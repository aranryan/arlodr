

#' Set scale to percent with one decimal place
#'
#' @param x
#' @param accuracy integer to determine the number of places to show after decimal point
#'
#' @return
#' @export
#'
#' @examples
#' In a ggplot plot, use the following to set to y-axis in percent with one decimals.
#' scale_y_continuous(labels = arlodr::scale_percent_1, limits=c(.02,NA))
#'
scale_percent_1 <- function(x) {
  # accuracy is setting the number to round to
  scales::percent(x, accuracy=.5)
}



#' Set scale to percent with no decimal places
#'
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' In a ggplot plot, use the following to set to y-axis in percent with no decimals.
#' #' scale_y_continuous(labels = arlodr::scale_percent_0, limits=c(.02,NA))
scale_percent_0 <- function(x) {
  # accuracy is setting the number to round to
  scales::percent(x, accuracy=1)
}

#' Set scale to percent with two decimal places
#'
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' #' In a ggplot plot, use the following to set to y-axis in percent with two decimals.
#' #' scale_y_continuous(labels = arlodr::scale_percent_2, limits=c(.02,NA))
scale_percent_2 <- function(x) {
  # accuracy is setting the number to round to
  scales::percent(x, accuracy=.01)
}
