

#' aggregates, or converts, from monthly to quarterly
#'
#' I based this on the as.quarterly function that is in the tframePlus package I
#' set it up so you have to give a type of aggregation it works for type=sum or
#' type=mean The ts line is basically converting a series from xts to ts,
#' because the ts works as an input to as.quarterly One reason that I believe I
#' needed to set this up was that I want to be able to run it accross columns of
#' an xts object, which one can't typically do with apply. also, the
#' aggregate.zoo function will sum up the months to a quarterly value even if
#' the last month is missing, for example:
#' tempc_m <- opcl_m$totusoprms
#' tempc_q <- aggregate(tempc_m, as.yearqtr, sum)
#' tempc_q <- xts(tempc_q)
#' so this is what I came up with
#'
#'
#' @param x
#' @param type
#'
#' @return
#' @export
#'
#' @examples
#'
#' # as an example of using this function is the steps I had in
#' #load_str_openclose, which are as follows
#' start <- as.yearqtr((start(opcl_m)))
#'h <- zooreg(vapply(opcl_m, m_to_q, FUN.VALUE =
#'                     numeric(floor(nrow(opcl_m)/3)),
#'                   type="sum"), start=start, frequency=4)
#'opcl_q <- xts(h)
#'indexClass(opcl_q) <- c("Date")
#'
#' a couple examples of as.quarterly for reference
#' z <- ts(1:10, start = c(1999,2), frequency=4)
#' z
#' as.annually(z)
#' as.annually(z, na.rm=TRUE)
#'
#' z <- ts(1:30, start = c(1999,2), frequency=12)
#' z
#' as.annually(z)
#' as.annually(z, na.rm=TRUE)
#' as.quarterly(z)
#' as.quarterly(z, na.rm=TRUE)

m_to_q=function(x, type){
  out_q <- tframePlus::as.quarterly(
    ts(as.numeric(x), frequency = 12, start = c(lubridate::year(start(x)), lubridate::month(start(x)))),
    FUN=type,
    # changed the following to FALSE. sometimes the dataframe will have some
    # series that start earlier than others. If I did TRUE, I think some were
    # being dropped, leading to errors when used in a vapply, where I had to
    # specify the length of what would come back, as it wasn't the same for all
    # so I changed this to FALSE, and then in the vapply I used ceiling to round
    # to include the last quarter
    na.rm=FALSE)
  return(out_q)
}

#' conversion to annual
#'
#' Similar to m_to_q
#'
#' @param x
#' @param type
#'
#' @return
#' @export
#'
#' @examples
q_to_a=function(x, type){
  a_a <- tframePlus::as.annually(
    ts(as.numeric(x), frequency = 4, start = c(lubridate::year(start(x)), lubridate::month(start(x)))),
    FUN=type,
    na.rm=FALSE)
  return(a_a)
}



#' takes a quarterly xts object with multiple columns and converts to annual
#'
#' need to give the type of conversion as argument, for example could do b <-
#' q_to_a_xts(suma, type="sum") this function uses the q_to_a function that I've
#' defined above, but then applys it to all of the columns of a xts object
#'
#' @param x
#' @param type
#'
#' @return
#' @export
#'
#' @examples
q_to_a_xts=function(x, type){
  start <- as.Date((start(x)))
  start <- as.numeric(format(start(x), "%Y"))
  h <- zooreg(vapply(x, q_to_a, FUN.VALUE =
                       numeric(floor(nrow(x)/4)),
                     type=type),  start=start, frequency=1)
  # at this point it has a four digit year as the index
  # but I wanted to format as a date with the start date of the year
  h <- zooreg(h, order.by=as.Date(paste(index(h),"-01-01", sep="")))
  h <- xts(h)
  return(h)
}
