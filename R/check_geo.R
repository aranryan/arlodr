

#' check whether all intended geographies are
#' included in a dataframe
#'
#' function to check whether all intended geographies are
#' included in a dataframe of series set up with var_geo
#' format series names

#' input a dataframe that is set up for eviews, for example,
#' with series names as column headers and dates in a date column
#' arguments: check_for = vector of codes to check for, such as based on
#' geo_for_eviews
#'
#' @param x
#' @param check_for
#'
#' @return
#' @export
#'
#' @examples
check_vargeo <- function(x, check_for) {
  all_series_1 <- colnames(x)[2:ncol(x)]

  temp_a1 <- x %>%
    gather(vargeo, value, -date) %>%
    # separates based on regular expression that finds last occurance of _
    separate(vargeo, c("var", "area_sh"), sep = "_(?!.*_)", extra="merge") %>%
    select(var) %>%
    distinct(var)

  temp_a2 <- expand.grid(x=temp_a1$var, y=unique(check_for)) %>%
    mutate(z = paste(x, y, sep="_")) %>%
    select(z)
  temp_a2 <- as.character(temp_a2$z)

  # all elements of the first vector without matching elements in the second
  # so taking temp_a2 as all combinations of the variables in the dataframe
  # with the areas in the check_for list. And then seeing if there are any
  # that aren't already in the dataframe.
  miss_series <- setdiff(temp_a2, all_series_1)
  if(length(miss_series)>0){
    # creates a dataframe with missing series as the column names
    df1 <- matrix(,nrow = 1, ncol = length(miss_series)) %>%
      data.frame()
    colnames(df1) <- miss_series
    # replace the 1's with NA
    df2 <- as.data.frame(lapply(df1, function(y) as.numeric(gsub("1", NA, y))))
    # creates a zoo object using the dates from the input data frame,
    # but then converts back to a dataframe
    df3 <- zoo(df2, order.by=x$date) %>%
      data.frame(date=index(.),.)
    # joins the dataframe of NAs for all missing series onto the input dataframe
    b <- left_join(x, df3, by = c("date" = "date"))
  }  else
    # if there is nothing in the missing series list, just return the original x
    b <- x
  output <- list(b, miss_series)
}
