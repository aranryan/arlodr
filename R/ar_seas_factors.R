

#' create monthly seasonal factors
#'
#' function takes a data frame of monthly str data and returns a set of monthly
#' seasonal factors, including future months
#'
#' @param str_m
#' @param dont_m_cols
#'
#' @return
#' @export
#'
#' @examples
seas_factors_m <- function(str_m, dont_m_cols){

  # gets ready to create monthly seasonal factors
  print("get ready to create monthly seasonal factors")

  # used to drop concepts that shouldn't be seasonally adjusted
  # but I removed it I put it into the actual script
  toadj_m <- str_m
  #   toadj_m <- select(str_m,
  #                     -ends_with("_days"),
  #                     -ends_with("_demt"),
  #                     -ends_with("_rmrevt"),
  #                     -ends_with("_supt"))

  # drop series that have given errors when seasonally adjusting in past
  #dont_m_cols <- c("anaheim_supd|dallas_supd|detroit_supd|neworleans_supd|oahu_supd|sanfrancisco_supd|tampa_supd")
  #dont_m_cols <- c("totus_demd|totus_occ|totus_revpar|totus_supd|anaheim_supd|neworleans_supd|oahu_supd|sanfranchisco_supd|tampa_supd")

  # put the matching series into a dataframe
  dontadj_m <- toadj_m %>%
    dplyr::select(date, matches(dont_m_cols))

  # if there is anything in addition to the date column in the dontadj_m data frame, then
  if (ncol(dontadj_m) > 1) {
    # also, removes the matching series from the full dataframe
    toadj_m  <- toadj_m  %>%
      dplyr::select(-matches(dont_m_cols))
    print("there are series being excluded from adjustment")
  } else {
    print("nothing in dataframe being excluded from adjustment")
  }
  #########
  #
  # create monthly seasonal factors
  #

  head(toadj_m)
  print("line37")

  # convert to xts
  toadj_m <- toadj_m %>%
    read.zoo(drop=FALSE) %>%
    xts()

  # creates a blank object with just a dummy series
  # this is used to hold the output of seasonal adjustment
  sf_m <- xts(order.by=index(toadj_m))
  sf_m <- merge(sf_m, dummy=1)

  # monthly seasonal adj of all columns in xts object
  # I would have used vapply, but hard to know how many rows in output
  for (i in 1:ncol(toadj_m)){
    current_series <- colnames(toadj_m[,i])
    print(paste("starting monthly adjustment of", current_series, sep=" "))
    temphold <- seasonal_ad(toadj_m[,i],
                            meffects =  c("const", "easter[8]", "thank[5]"))
    sf_m <- merge(sf_m, temphold)
  }

  print("line56")


  # turn into a data frame with a date column
  sf_m <- data.frame(date=time(sf_m), sf_m)
  sf_m$date <- as.Date(sf_m$date)
  row.names(sf_m) <- NULL
  # full copy of seasonally adjusted, factors, and irreg
  full_sf_m <- dplyr::select(sf_m, -dummy)
  # takes just the seasonal factors
  sf_m <- sf_m %>%
    dplyr::select(date, ends_with("_sf"))

  #########
  #
  # create seasonal factors for those that were specifically not adjusted

  temp_a <- sf_m %>%
    read.zoo(drop=FALSE) %>%
    xts()
  tempc <- index(temp_a)
  head(tempc)

  print("line76")
  # create a matrix of ones with sufficient number of columns
  m <- matrix(rep(1), nrow = nrow(sf_m), ncol = ncol(dontadj_m)) %>%
    data.frame()
  # get column names, add sf to names for seasonal factor
  colnames(m) <- paste(colnames(dontadj_m),"_sf", sep="")
  # drop the date_sf column
  m <- select(m, -date_sf)
  # create dataframe with date as first column
  sf_m_dontadj <- cbind(tempc, m) %>%
    dplyr::rename(date = tempc)

  print("line87")
  ########
  #
  # combines seasonal factors from adjustments and skipped
  #

  sf_m <- merge(sf_m, sf_m_dontadj, by= "date")

  return(sf_m)
}


#' create quarterly seasonal factors
#'
#' function takes a data frame of quarterly str data and returns a set of
#' quarterly seasonal factors, including future months
#'
#' @param str_q
#' @param dont_q_cols
#'
#' @return
#' @export
#'
#' @examples
seas_factors_q <- function(str_q, dont_q_cols){

  # gets ready to create quarterly seasonal factors
  print("get ready to create quarterly seasonal factors")

  # used to drop concepts that shouldn't be seasonally adjusted
  # but I removed it I put it into the actual script
  toadj_q <- str_q

  #   toadj_q <- select(str_q,
  #                     -ends_with("_days"),
  #                     -ends_with("_demt"),
  #                     -ends_with("_rmrevt"),
  #                     -ends_with("_supt"))

  # drop series that have given errors when seasonally adjusting in past
  #dont_q_cols <- c("anaheim_supd|neworleans_supd|oahu_supd|sanfranchisco_supd|tampa_supd")
  #dont_q_cols <- c("totus_demd|totus_occ|totus_revpar|totus_supd|anaheim_supd|neworleans_supd|oahu_supd|sanfranchisco_supd|tampa_supd")

  # put the matching series into a dataframe
  dontadj_q <- toadj_q %>%
    dplyr::select(date, matches(dont_q_cols))

  # if there is anything in addition to the date column in the dontadj_m data frame, then
  if (ncol(dontadj_q) > 1) {
    # also, removes the matching series from the full dataframe
    toadj_q  <- toadj_q  %>%
      dplyr::select(-matches(dont_q_cols))
    print("there are series being excluded from adjustment")
  } else {
    print("nothing in dataframe being excluded from adjustment")
  }

  #########
  #
  # create quarterly seasonal factors
  #

  # convert to xts
  toadj_q <- toadj_q %>%
    read.zoo(drop=FALSE) %>%
    xts()

  # creates a blank object with just a dummy series
  # this is used to hold the output of seasonal adjustment
  sf_q <- xts(order.by=index(toadj_q))
  sf_q <- merge(sf_q, dummy=1)

  # quarterly seasonal adj of all columns in xts object
  # I would have used vapply, but hard to know how many rows in output
  for (i in 1:ncol(toadj_q)){
    current_series <- colnames(toadj_q[,i])
    print(paste("starting quarterly adjustment of", current_series, sep=" "))
    # take out thanksgiving variable for quarterly
    temphold <- seasonal_ad(toadj_q[,i],
                            qeffects =  c("const", "easter[8]"))
    sf_q <- merge(sf_q, temphold)
  }

  # turn into a data frame with a date column
  sf_q <- data.frame(date=time(sf_q), sf_q)
  sf_q$date <- as.Date(sf_q$date)
  row.names(sf_q) <- NULL
  # full copy of seasonally adjusted, factors, and irreg
  full_sf_q <- dplyr::select(sf_q, -dummy)
  # takes just the seasonal factors
  sf_q <- sf_q %>%
    dplyr::select(date, ends_with("_sf"))

  #########
  #
  # create seasonal factors for those that were specifically not adjusted

  temp_a <- sf_q %>%
    read.zoo(drop=FALSE) %>%
    xts()
  tempc <- index(temp_a)
  head(tempc)

  # create a matrix of ones with sufficient number of columns
  m <- matrix(rep(1), nrow = nrow(sf_q), ncol = ncol(dontadj_q)) %>%
    data.frame()
  # get column names, add sf to names for seasonal factor
  colnames(m) <- paste(colnames(dontadj_q),"_sf", sep="")
  # drop the date_sf column
  m <- dplyr::select(m, -date_sf)
  # create dataframe with date as first column
  sf_q_dontadj <- cbind(tempc, m) %>%
    dplyr::rename(date = tempc)

  ########
  #
  # combines seasonal factors from adjustments and skipped
  #

  sf_q <- merge(sf_q, sf_q_dontadj, by= "date")

  return(sf_q)
}


#' modifed version of seas_factors_m
#'
#' @param str_m
#' @param dont_m_cols
#' @param hold_reg
#'
#' @return
#' @export
#'
#' @examples
seas_factors_m_2 <- function(str_m, dont_m_cols, hold_reg){

  # gets ready to create monthly seasonal factors
  print("get ready to create monthly seasonal factors")

  # used to drop concepts that shouldn't be seasonally adjusted
  # but I removed it I put it into the actual script
  toadj_m <- str_m
  #   toadj_m <- select(str_m,
  #                     -ends_with("_days"),
  #                     -ends_with("_demt"),
  #                     -ends_with("_rmrevt"),
  #                     -ends_with("_supt"))

  # drop series that have given errors when seasonally adjusting in past
  #dont_m_cols <- c("anaheim_supd|dallas_supd|detroit_supd|neworleans_supd|oahu_supd|sanfrancisco_supd|tampa_supd")
  #dont_m_cols <- c("totus_demd|totus_occ|totus_revpar|totus_supd|anaheim_supd|neworleans_supd|oahu_supd|sanfranchisco_supd|tampa_supd")

  # put the matching series into a dataframe
  dontadj_m <- toadj_m %>%
    select(date, matches(dont_m_cols))

  # if there is anything in addition to the date column in the dontadj_m data frame, then
  if (ncol(dontadj_m) > 1) {
    # also, removes the matching series from the full dataframe
    toadj_m  <- toadj_m  %>%
      select(-matches(dont_m_cols))
    print("there are series being excluded from adjustment")
  } else {
    print("nothing in dataframe being excluded from adjustment")
  }
  #########
  #
  # create monthly seasonal factors
  #

  head(toadj_m)
  print("line37")

  # convert to xts
  toadj_m <- toadj_m %>%
    read.zoo(drop=FALSE) %>%
    xts()

  # creates a blank object with just a dummy series
  # this is used to hold the output of seasonal adjustment
  sf_m <- xts(order.by=index(toadj_m))
  sf_m <- merge(sf_m, dummy=1)

  # monthly seasonal adj of all columns in xts object
  # I would have used vapply, but hard to know how many rows in output
  for (i in 1:ncol(toadj_m)){
    current_series <- colnames(toadj_m[,i])
    print(paste("starting monthly adjustment of", current_series, sep=" "))
    temphold <- seasonal_ad_2(toadj_m[,i],
                              meffects =  c("tdnolpyear", "ls2001.Sep"),
                              hold_reg = hold_reg)
    sf_m <- merge(sf_m, temphold)
  }

  print("line56")


  # turn into a data frame with a date column
  sf_m <- data.frame(date=time(sf_m), sf_m)
  sf_m$date <- as.Date(sf_m$date)
  row.names(sf_m) <- NULL
  # full copy of seasonally adjusted, factors, and irreg
  full_sf_m <- select(sf_m, -dummy)
  # takes just the seasonal factors
  sf_m <- sf_m %>%
    select(date, ends_with("_sf"))

  #########
  #
  # create seasonal factors for those that were specifically not adjusted

  temp_a <- sf_m %>%
    read.zoo(drop=FALSE) %>%
    xts()
  tempc <- index(temp_a)
  head(tempc)

  print("line76")
  # create a matrix of ones with sufficient number of columns
  m <- matrix(rep(1), nrow = nrow(sf_m), ncol = ncol(dontadj_m)) %>%
    data.frame()
  # get column names, add sf to names for seasonal factor
  colnames(m) <- paste(colnames(dontadj_m),"_sf", sep="")
  # drop the date_sf column
  m <- select(m, -date_sf)
  # create dataframe with date as first column
  sf_m_dontadj <- cbind(tempc, m) %>%
    dplyr::rename(date = tempc)

  print("line87")
  ########
  #
  # combines seasonal factors from adjustments and skipped
  #

  sf_m <- merge(sf_m, sf_m_dontadj, by= "date")

  return(sf_m)
}
