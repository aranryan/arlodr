




#' return days in month
#'
#' I modified so you can set leap_impact equal 0 to help with str data
#'
#' @param d
#' @param leap_impact
#'
#' @return
#' @export
#'
#' @examples
#' days_in_month("2014-11-25")
days_in_month <- function(d = Sys.Date(), leap_impact=1){

  m = substr((as.character(d)),6,7)              # month number as string
  y = as.numeric(substr((as.character(d)),1,4))  # year number as numeric

  # Quick check for leap year
  leap = 0
  if ((y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0){leap = leap_impact}

  # Return the number of days in the month
  return(switch(m,
                '01' = 31,
                '02' = 28 + leap,
                '03' = 31,
                '04' = 30,
                '05' = 31,
                '06' = 30,
                '07' = 31,
                '08' = 31,
                '09' = 30,
                '10' = 31,
                '11' = 30,
                '12' = 31))
}

