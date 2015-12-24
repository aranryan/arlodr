

#' process monthly STR data
#'
#'function takes a data frame of monthly str data
#' and returns a list containing a monthly data frame
#' and a quarterly data frame
#'
#' @param load_m
#'
#' @return
#' @export
#'
#' @examples
load_str <- function(load_m){

  # spreads into a tidy format with
  # tidyr and then calculates the occupancy and revpar series
  # first needs to go from xts to dataframe
  # b1 <- data.frame(date=time(lodus_m), lodus_m) %>%
  b1 <- load_m %>%
    # creates column called segvar that contains the column names, and one next to
    # it with the values, dropping the time column
    gather(segvar, value, -date, na.rm = FALSE) %>%
    # in the following the ^ means anything not in the list
    # with the list being all characters and numbers
    # so it separates segvar into two colums using sep

    # in August 2015 I changed the following line to the one below it
    # which separates based on the last occurance of an underscore
    # I changed as part of the host data, in which I had a few underscores in my
    # series name and wanted to split on the last one, I hope this works generally
    # also changed quarterly below
    # separate(segvar, c("seg", "variable"), sep = "[^[:alnum:]]+") %>%
    separate(segvar, c("seg", "variable"), sep = "_(?!.*_)", extra="merge") %>%

    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    spread(variable,value) %>%
    # days_in_month is a function I borrowed. leap_impact=0 ignores leap year
    # this uses transform to create a new column where the new column is
    # created by using sapply on the date column to apply the days_in_month
    # function with the leap_impact argument set to 0
    transform(days = sapply(date, days_in_month,leap_impact=0)) %>%
    # adds several new calculated columns
    mutate(occ = demt / supt) %>%
    mutate(revpar = rmrevt / supt) %>%
    mutate(adr = rmrevt / demt) %>%
    # converts several concepts to millions
    mutate(supt = supt / 1000000) %>%
    mutate(demt = demt / 1000000) %>%
    mutate(rmrevt = rmrevt / 1000000) %>%
    mutate(demd = demt / days) %>%
    mutate(supd = supt / days)

  load_m <- b1

  #############################
  #
  # creates quarterly by summing monthly
  #

  # get it ready to convert
  # takes it from a tidy format and melts it creating a dataframe with the
  # following columns (date, seg, variable, value), and then creates the unique
  # variable names and then reads into a zoo object spliting on the
  # second column
  m_z <- load_m %>%
    select(-occ, -adr, -revpar, -demd, -supd) %>%
    melt(id=c("date","seg"), na.rm=FALSE) %>%
    mutate(variable = paste(seg, "_", variable, sep='')) %>%
    select(-seg) %>%
    read.zoo(split = 2)

  # convert to quarterly
  # I couldn't use apply because the object is
  # a xts, not a dataframe, see
  # http://codereview.stackexchange.com/questions/39180/best-way-to-apply-across-an-xts-object

  # sets up the start of the index that will be used for the quarterly object
  # uses vapply to essentially run an apply across the xts object because
  # apply doesn't work on an xts object
  # for vapply we need to give the expected length in FUN.VALUE and a
  # start date and quarterly frequency
  # The function that I'm applying to each column is m_to_q, which I wrote, the type="sum"
  # is giving the type of aggregation to use in it

  # as a temp fix, I shortened a_mz to end at the end of a quarter.
  # I need to come up with a better fix. The issue was that the
  # function was expecting something length 111, but getting 112.
  # might be an issue because I changed na.rm to FALSE in the
  # m_to_q function because it was causing issues for the Mexico
  # series that were different lengths. So I put that back to na.rm=TRUE and it worked


  # head(raw_str_us)
  # head(tempa)
  # tempa <- read.zoo(raw_str_us)
  # head(tempa)
  # tempd <- tempa$totus_demt
  # str(tempd)
  # tail(tempd)
  # tempd2 <- m_to_q(tempd,type=sum)
  # tempd2 <- zoo(tempd2)
  # tail(tempd2)
  # str(tempd2)
  #
  # nrow(tempd2)
  # nrow(tempa)/3
  # ceiling(nrow(tempa)/3)
  # start <- as.yearqtr((start(tempa)))
  #
  # temp_q <- zooreg(vapply(tempa, m_to_q, FUN.VALUE =
  #                           numeric(ceiling(nrow(tempa)/3)),
  #                         type="sum"), start=start, frequency=4)
  # head(temp_q)
  # tail(temp_q)


  start <- as.yearqtr((start(m_z)))
  load_q <- zooreg(vapply(m_z, m_to_q, FUN.VALUE =
                            numeric(ceiling(nrow(m_z)/3)),
                          type="sum"), start=start, frequency=4)
  head(load_q)

  # turn into a data frame with a date column
  load_q <- data.frame(date=time(load_q), load_q)
  load_q$date <- as.Date(load_q$date)
  row.names(load_q) <- NULL

  # goes into tidy format and then adds some calculated series
  b1q <- load_q %>%
    # creates column called segvar that contains the column names, and one next to
    # it with the values, dropping the time column
    gather(segvar, value, -date, na.rm = FALSE) %>%

    # in August 2015 I changed the following line to the one below it
    # which separates based on the last occurance of an underscore
    # I changed as part of the host data, in which I had a few underscores in my
    # series name and wanted to split on the last one, I hope this works generally
    # also changed quarterly below
    # separate(segvar, c("seg", "variable"), sep = "[^[:alnum:]]+") %>%
    separate(segvar, c("seg", "variable"), sep = "_(?!.*_)", extra="merge") %>%

    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    spread(variable,value) %>%
    # adds several new calculated columns
    mutate(occ = demt / supt) %>%
    mutate(revpar = rmrevt / supt) %>%
    mutate(adr = rmrevt / demt) %>%
    mutate(demd = demt / days) %>%
    mutate(supd = supt / days)

  load_q <- b1q

  #############################
  #
  # puts back to wide format
  #

  # puts it back into a wide data frame, with one column for each series
  # days is a series for each segment/market\
  load_m <- load_m %>%
    melt(id=c("date","seg"), na.rm=FALSE) %>%
    mutate(variable = paste(seg, "_", variable, sep='')) %>%
    select(-seg) %>%
    spread(variable,value)
  # if instead I had wanted a zoo object, I could have done
  #read.zoo(split = 2)

  # converts to xts from dataframe
  #lodus_m <- lodus_m %>%
  #  read.zoo() %>%
  #  as.xts

  # puts it back into a wide zoo object, with one column for each series
  # days is a series for each segment/market\
  load_q <- load_q %>%
    melt(id=c("date","seg"), na.rm=FALSE) %>%
    mutate(variable = paste(seg, "_", variable, sep='')) %>%
    select(-seg) %>%
    spread(variable,value)
  # if instead I had wanted a zz object, I could have done
  #read.zoo(split = 2

  return(list(load_m,load_q))
}
