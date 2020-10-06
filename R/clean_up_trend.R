#' Cleans up a dataframe of data that has been read in from the Raw Data tab of a monthly trend report
#'
#' @param df Source data
#' @param seggeo Character string to name the segment or geography of data
#' @param trend_type If "international", expects an exchange rate column to be in the source data
#'
#' @return
#' @export
#'
#' @examples
clean_up_trend <- function(df,seggeo, trend_type) {
  # df is the raw data read in from Excel
  # seggeo is a character string to be used to name the metro


  # Set up -------------------------
  # set up two character vectors of the columns to select
  # Depends on whether it is an international trend report
  # with an exchange rate column, or domestic.
  col_vector <- c("date", "supply", "demand", "revenue", "censprops", "censrooms", "censparticip")
  if(trend_type=='international'){
    col_vector <- c(col_vector, "exchange_rate")
  }
  col_vector_drop <- c("-censprops", "-censrooms", "-censparticip", "-occ", "-adr", "-revpar")
  if(trend_type=='international'){
    col_vector_drop <- c(col_vector_drop, "-exchange_rate")
  }
  # set up number of expected columns based on whether it is an
  # international trend report with an exchange rate column or not
  if(trend_type=='international'){
    exp_col_number = 17
  } else {
    exp_col_number = 16
  }

  # Process -----------------------
  input_1 <- df

  input_2 <- input_1 %>%
    # drop columns that are all NA
    .[, colSums(is.na(.)) != nrow(.)]

  # check column names equal expected length
  a <- length(colnames(input_2))
  stopifnot(a == exp_col_number)

  # delete the second row
  input_2 <- input_2[-c(1), ]

  # initial clean up of column names
  colnames(input_2) <- colnames(input_2) %>%
    tolower() %>%
    gsub(" ", "_", .)

  # name certain columns based on position, which is maybe not a
  # great, stable idea.
  names(input_2)[3] <- "x3"
  names(input_2)[5] <- "x5"
  names(input_2)[7] <- "x7"
  names(input_2)[9] <- "x9"
  names(input_2)[11] <- "x11"
  names(input_2)[13] <- "x13"
  names(input_2)[14] <- "censprops"
  names(input_2)[15] <- "censrooms"
  names(input_2)[16] <- "censparticip"

  # select columns we want to keep
  input_3 <- input_2 %>%
    # drop any rows that are NA in date column
    filter(!is.na(date))	%>%
    select_(., .dots = col_vector) %>%
    rename(supt=supply, demt=demand, rmrevt=revenue) %>%
    #select(date, supt=supply, demt=demand, rmrevt=revenue, censprops, censrooms, censparticip, exchange_rate) %>%
    mutate(supt = as.numeric(supt),
           demt = as.numeric(demt),
           rmrevt = as.numeric(rmrevt)) %>%
    mutate(occ = demt/supt) %>%
    mutate(adr = rmrevt/demt) %>%
    mutate(revpar = rmrevt/supt) %>%
    # format date column
    mutate(date = zoo::as.yearmon(date, format = "%b %y")) %>%
    mutate(date = zoo::as.Date(date))

  # insert seggeo
  input_4 <- input_3 %>%
    mutate(seggeo = seggeo) %>%
    select(date, seggeo, everything())

  # select a few series to drop
  input_5 <- input_4 %>%
    #select(-censprops, -censrooms, -censparticip, -exchange_rate, -occ, -adr, -revpar) %>%
    select_(., .dots = col_vector_drop) %>%
    filter(!(is.na(date)))
}
