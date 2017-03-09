

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
load_str <- function(load_m, units=NULL){


  ####
  # defines units measure
  # default value for unit_div is one million, so room demand and
  # supply and room revenue will be divided by 1 million.
  if(is.null(units)) unit_div <- 1000000

  # but then can also use specific measures
  if(!is.null(units)) {
    if(units == "millions") unit_div <- 1000000
    if(units == "thousands") unit_div <- 1000
    if(units == "ones") unit_div <- 1
  }
print(unit_div)

  # spreads into a tidy format with
  # tidyr and then calculates the occupancy and revpar series
  # first needs to go from xts to dataframe
  # b1 <- data.frame(date=time(lodus_m), lodus_m) %>%
  b1 <- load_m %>%
    # creates column called segvar that contains the column names, and one next to
    # it with the values, dropping the time column
    tidyr::gather(segvar, value, -date, na.rm = FALSE) %>%
    # in the following the ^ means anything not in the list
    # with the list being all characters and numbers
    # so it separates segvar into two colums using sep

    # in August 2015 I changed the following line to the one below it
    # which separates based on the last occurance of an underscore
    # I changed as part of the host data, in which I had a few underscores in my
    # series name and wanted to split on the last one, I hope this works generally
    # also changed quarterly below
    # separate(segvar, c("seg", "variable"), sep = "[^[:alnum:]]+") %>%
    tidyr::separate(segvar, c("seg", "variable"), sep = "_(?!.*_)", extra="merge") %>%

    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    tidyr::spread(variable,value) %>%
    # days_in_month is a function I borrowed. leap_impact=0 ignores leap year
    # this uses transform to create a new column where the new column is
    # created by using sapply on the date column to apply the days_in_month
    # function with the leap_impact argument set to 0
    base::transform(days = sapply(date, days_in_month,leap_impact=0)) %>%
    # adds several new calculated columns
    dplyr::mutate(occ = demt / supt) %>%
    dplyr::mutate(revpar = rmrevt / supt) %>%
    dplyr::mutate(adr = rmrevt / demt) %>%
    # converts several concepts to millions
    dplyr::mutate(supt = supt / unit_div) %>%
    dplyr::mutate(demt = demt / unit_div) %>%
    dplyr::mutate(rmrevt = rmrevt / unit_div) %>%
    dplyr::mutate(demd = demt / days) %>%
    dplyr::mutate(supd = supt / days)

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
    dplyr::select(-occ, -adr, -revpar, -demd, -supd) %>%
    reshape2::melt(id=c("date","seg"), na.rm=FALSE) %>%
    dplyr::mutate(variable = paste(seg, "_", variable, sep='')) %>%
    dplyr::select(-seg) %>%
    zoo::read.zoo(split = 2)

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


  start <- zoo::as.yearqtr((start(m_z)))
  load_q <- zoo::zooreg(vapply(m_z, arlodr::m_to_q, FUN.VALUE =
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
    tidyr::gather(segvar, value, -date, na.rm = FALSE) %>%

    # in August 2015 I changed the following line to the one below it
    # which separates based on the last occurance of an underscore
    # I changed as part of the host data, in which I had a few underscores in my
    # series name and wanted to split on the last one, I hope this works generally
    # also changed quarterly below
    # separate(segvar, c("seg", "variable"), sep = "[^[:alnum:]]+") %>%
    tidyr::separate(segvar, c("seg", "variable"), sep = "_(?!.*_)", extra="merge") %>%

    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    tidyr::spread(variable,value) %>%
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
    reshape2::melt(id=c("date","seg"), na.rm=FALSE) %>%
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
    reshape2::melt(id=c("date","seg"), na.rm=FALSE) %>%
    mutate(variable = paste(seg, "_", variable, sep='')) %>%
    select(-seg) %>%
    spread(variable,value)
  # if instead I had wanted a zz object, I could have done
  #read.zoo(split = 2

  return(list(load_m,load_q))
}


#' Process monthly STR data
#'
#' Previously this was the first part of load_str, which had a second part that
#' did the quarterly steps. I've broken it apart in July 2016 so that I could
#' make tweaks to the quarterly part, and it seemed unnecessary to keep all
#' together as one step.
#'
#' @param load_m Takes a wide dataframe with monthly lodging date as an input (e.g. with
#' each series in a column).
#' @param units "millions", "thousands" or "ones" Will be used to divide
#' supt, demt, rmrevt, supd, and demd.
#'
#' @return Returns a wide dataframe with the processed
#' STR data. It will contain:
#' adr
#' days
#' demd
#' demt
#' occ
#' revpar
#' rmrevt
#' supd
#' supt
#'
#' @export
#'
#' @examples
load_str_m <- function(load_m, units=NULL){

  # takes a monthly dataframe as an input

  ####
  # defines units measure
  # default value for unit_div is one million, so room demand and
  # supply and room revenue will be divided by 1 million.
  if(is.null(units)) unit_div <- 1000000

  # but then can also use specific measures
  if(!is.null(units)) {
    if(units == "millions") unit_div <- 1000000
    if(units == "thousands") unit_div <- 1000
    if(units == "ones") unit_div <- 1
  }
  print(paste0("unit divisor is ", unit_div))

  # spreads into a tidy format with
  # tidyr and then calculates the occupancy and revpar series
  # first needs to go from xts to dataframe
  # b1 <- data.frame(date=time(lodus_m), lodus_m) %>%
  b1 <- load_m %>%
    # creates column called segvar that contains the column names, and one next to
    # it with the values, dropping the time column
    tidyr::gather(segvar, value, -date, na.rm = FALSE) %>%
    # in the following the ^ means anything not in the list
    # with the list being all characters and numbers
    # so it separates segvar into two colums using sep

    # in August 2015 I changed the following line to the one below it
    # which separates based on the last occurance of an underscore
    # I changed as part of the host data, in which I had a few underscores in my
    # series name and wanted to split on the last one, I hope this works generally
    # also changed quarterly below
    # separate(segvar, c("seg", "variable"), sep = "[^[:alnum:]]+") %>%
    tidyr::separate(segvar, c("seg", "variable"), sep = "_(?!.*_)", extra="merge") %>%

    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    tidyr::spread(variable,value) %>%
    # days_in_month is a function I borrowed. leap_impact=0 ignores leap year
    # this uses transform to create a new column where the new column is
    # created by using sapply on the date column to apply the days_in_month
    # function with the leap_impact argument set to 0
    base::transform(days = sapply(date, days_in_month,leap_impact=0)) %>%
    # adds several new calculated columns
    dplyr::mutate(occ = demt / supt) %>%
    dplyr::mutate(revpar = rmrevt / supt) %>%
    dplyr::mutate(adr = rmrevt / demt) %>%
    # converts several concepts to millions
    dplyr::mutate(supt = supt / unit_div) %>%
    dplyr::mutate(demt = demt / unit_div) %>%
    dplyr::mutate(rmrevt = rmrevt / unit_div) %>%
    dplyr::mutate(demd = demt / days) %>%
    dplyr::mutate(supd = supt / days)

  # puts back to wide format, with one column for each series
  load_m <- b1 %>%
    tidyr::gather(variable, value, -date, -seg) %>%
    tidyr::unite(segvar, seg, variable, sep="_", remove=TRUE) %>%
    tidyr::spread(segvar, value)
}

#' Process monthly STR data to create quarterly data
#'
#' This was originally the second part of the load_str step. I broke that
#' apart. I also added to this so that it creates some end-of-period,
#' and start-of-period supply data.
#'
#' @param load_m Monthly STR data, after it is processed in load_str_m
#'
#' @return Dataframe with quarterly STR data. In addition to the series in the
#' monthly output, which are:
#' adr
#' days
#' demd
#' demt
#' occ
#' revpar
#' rmrevt
#' supd
#' supt
#' It will also contain:
#' sups (start-of-period supply, based on the first month of the quarter)
#' supe (end-of-period supply, based on first month of following quarter)
#' supem (ending month supply, supply in last month of quarter)
#'
#'
#' @export
#'
#' @examples
load_str_q <- function(load_m){

  # puts into tidy format
  load_m_tidy <- load_m %>%
    gather(segvar, value, -date) %>%
    # separates based on last occurance of _
    separate(segvar, c("seggeo", "variable"), sep = "_(?!.*_)", extra="merge") %>%
    spread(variable, value)

  #############################
  #
  # creates quarterly by summing monthly
  #

  # get it ready to convert
  # takes it from a tidy format and melts it creating a dataframe with the
  # following columns (date, seg, variable, value), and then creates the unique
  # variable names and then reads into a zoo object spliting on the
  # second column
  m_z <- load_m_tidy  %>%
    dplyr::select(-occ, -adr, -revpar, -demd, -supd) %>%
    reshape2::melt(id=c("date","seggeo"), na.rm=FALSE) %>%
    dplyr::mutate(variable = paste(seggeo, "_", variable, sep='')) %>%
    dplyr::select(-seggeo) %>%
    zoo::read.zoo(split = 2)

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



  start <- zoo::as.yearqtr((start(m_z)))
  load_q <- zoo::zooreg(vapply(m_z, arlodr::m_to_q, FUN.VALUE =
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
    tidyr::gather(segvar, value, -date, na.rm = FALSE) %>%

    # in August 2015 I changed the following line to the one below it
    # which separates based on the last occurance of an underscore
    # I changed as part of the host data, in which I had a few underscores in my
    # series name and wanted to split on the last one, I hope this works generally
    # also changed quarterly below
    # separate(segvar, c("seg", "variable"), sep = "[^[:alnum:]]+") %>%
    tidyr::separate(segvar, c("seg", "variable"), sep = "_(?!.*_)", extra="merge") %>%

    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    tidyr::spread(variable,value) %>%
    # adds several new calculated columns
    mutate(occ = demt / supt) %>%
    mutate(revpar = rmrevt / supt) %>%
    mutate(adr = rmrevt / demt) %>%
    mutate(demd = demt / days) %>%
    mutate(supd = supt / days)

  ###########
  #
  # create end-of-period supply, and start-of-period supply based on monthly data


  # use monthly supply to calculate start of quarter supply
  # this approach is based on the help for aggregate.zoo
  # function which returns corresponding first "Date" of quarter
  first.of.quarter <- function(tt) as.Date(as.yearqtr(tt))

  temp_a <- load_m_tidy %>%
    tidyr::gather(variable, value, -date, -seggeo) %>%
    filter(variable == "supd") %>%
    tidyr::unite(segvar, seggeo, variable, sep="_", remove=TRUE) %>%
    tidyr::spread(segvar, value)

  # this creates an object with quarterly data for sups. The value for
  # the quarter is the value from the first month of supd for the quarter.
  temp_sups <- temp_a %>%
    # sort of odd, but to get the aggregate step to retain column names
    # in situations in which there is only one series, I had to add
    # a blank series, and then delete it shortly thereafter.
    mutate(blank=as.numeric(1)) %>%
    read.zoo(drop=FALSE) %>%
    as.xts %>%
    aggregate(., first.of.quarter, first, regular=TRUE) %>%
    as.xts %>%
    # work to rename by putting in tidy format temporarily
    data.frame(date=time(.), .) %>%
    select(-blank) %>%
    gather(segvar, value, -date) %>%
    mutate(segvar = gsub("_supd", "_sups", segvar)) %>%
    spread(segvar, value)

  # end of quarter supply, based on start of quarter supply from the next quarter
  temp_supe <- temp_sups %>%
    mutate_if(is.numeric, funs(lead(., 1))) %>%
    # work to rename by putting in tidy format temporarily
    gather(segvar, value, -date) %>%
    mutate(segvar = gsub("_sups", "_supe", segvar)) %>%
    spread(segvar, value)

  # supply in the last month of the quarter (ending month supply)
  # This has the side effect of using data for the final month of the quarter,
  # even if we don't have all three months of the quarter. That's unfortunate. It
  # is accurate, in that it has the data for the last month available in the quarter,
  # but it's not really a final observation for that quarter, so it could be troublesome.
  temp_supem <- temp_a %>%
    # sort of odd, but to get the aggregate step to retain column names
    # in situations in which there is only one series, I had to add
    # a blank series, and then delete it shortly thereafter.
    mutate(blank=as.numeric(1)) %>%
    read.zoo(drop=FALSE) %>%
    as.xts %>%
    aggregate(., first.of.quarter, last, regular=TRUE) %>%
    as.xts %>%
    # work to rename by putting in tidy format temporarily
    data.frame(date=time(.), .) %>%
    select(-blank) %>%
    gather(segvar, value, -date) %>%
    mutate(segvar = gsub("_supd", "_supem", segvar)) %>%
    spread(segvar, value)

  # combine the data and put in tidy format
  temp_b <- left_join(temp_sups, temp_supe, by=("date")) %>%
    left_join(temp_supem, by=("date")) %>%
    gather(segvar, value, -date) %>%
    separate(segvar, c("seg", "variable"), sep = "_(?!.*_(?!s.$))", extra="merge") %>%
    spread(variable, value)

  ##########
  #
  # combine the additional supply series with the rest of the quarterly data

  b2q <- left_join(b1q, temp_b, by=c("date", "seg"))

  # puts back to wide format, with one column for each series
  load_q <- b2q %>%
    tidyr::gather(variable, value, -date, -seg) %>%
    tidyr::unite(segvar, seg, variable, sep="_", remove=TRUE) %>%
    tidyr::spread(segvar, value)

}
