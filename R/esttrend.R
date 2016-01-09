#' Estimate a trend
#'
#' Can be based on log of original series or linear. Extracts trend term
#' coefficient. Works on an xts object, maybe also zoo.
#'
#' @param x
#' @param form
#'
#' @return
#' @export
#'
#' @examples
esttrend <- function (x, form) {
  if (form=="log") {
    df <- x %>%
      data.frame(date=time(.), .)
      #I had set up to add a trend variable called trendint because for a while
      #I couldn't figure out how to get it wo work with a time trend. But then
      #all of sudden it seemed to work. I guess part of the difference was doing
      #time(date), rather than time(x), which somehow uses the time of the xts
      #object. Whereas time(date), it turns out, gives you a integer time trend.
      #mutate(trendint = 1:nrow(.))
    mod <- lm(log(x) ~ time(date), data = df)
    res <- coef(summary(mod))
    res2 <- sapply(res, "[" ,2  ,"Estimate")
    # annualize
    res3 <- (1 + res2)^4 -1
    res3
  } else
    if (form=="linear") {
      # not set up yet
      c("NA")
    } else {
      c("NA")
    }
}

#' Drop columns that are all na
#'
#' Works on an xts object. Useful to run before a rollapply regression.
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
xts_dropallna <- function (x) {
  data.frame(date=time(x), x) %>%
  Filter(function(y)!all(is.na(y)), .) %>%
  read.zoo(regular=TRUE, drop=FALSE) %>%
  xts()
}


