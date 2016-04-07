################################
#
# this is another approach:
# idea is to create list of what is available
# pull that, code it appropriately, and then work with it from there

# example of steps for total employment
# attempt to pull individual variables across all msas
# so vision is to create data frames that have the individual variable across
# all msas for which it is available
# process is to creat a list of MSA codes, and then pull the series for those
# msa codes. So constructing the structure of the pull as a way of ensuring
# the right series are being pulled


# defines it as a function
#' Get a Fred table list
#'
#' @param search_tags1
#' @param search_tags2
#' @param freq_var
#' @param drop_title
#' @param keep_title
#' @param series_id
#' @param series_id_gsubend
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
getFredTable_list_aranmod <- function(search_tags1,
                                      search_tags2,
                                      freq_var,
                                      drop_title,
                                      keep_title,
                                      series_id,
                                      series_id_gsubend, ...) {
  # finds series that meet certain tag criteria
  tempa1 <- fred$tags.series(search_tags1)
  # finds other series based on additional tag criteria, such as necta
  # checks to see if user defined search_tags2, if so
  # then it does search and binds on the results
  my_args <- match.call()
  tags2 <- ("search_tags2" %in% names(my_args))
  if(tags2) {
    tempa2 <- fred$tags.series(search_tags2)
    tempa1 <- rbind(tempa1,tempa2)
  }
  tempb <- tempa1 %>%
    filter(frequency==freq_var) %>%
    # drops any that start with either of these two words
    filter(!grepl(drop_title, title)) %>%
    # keeps only those that match
    filter(grepl(keep_title,title)) %>%
    filter(grepl(series_id_gsubend, id))
  fred_ces_msa_list1 <- tempb %>%
    select(id,title) %>%
    mutate(msa_code = gsub(series_id_gsubend, "", id))
  return(fred_ces_msa_list1)
}

#########################
#
# testing version. ok to delete

# temp_l <- list(
#   search_tags1 = "employment;nonfarm;sa;monthly;msa",
#   search_tags2 = "employment;nonfarm;sa;monthly;necta",
#   freq_var = "Monthly",
#   drop_title = "^Change",
#   keep_title = "All Employees: Total Nonfarm",
#   series_id = "NA",
#   series_id_gsubend = paste("NA","$", sep=""),
#   ac_substr_1=as.integer("1"),
#   ac_substr_2=as.integer("-3"),
#   var_substr_1=as.integer("-2"),
#   var_substr_2=as.integer("-1"))
#
# str(temp_l)
#
# testfun <- function(search_tags1, search_tags2, freq_var, drop_title, keep_title,...) {
#   # finds series that meet certain tag criteria
#   tempa1 <- fred$tags.series(search_tags1)
#   # finds other series based on additional tag criteria, such as necta
#   # checks to see if user defined search_tags2, if so
#   # then it does search and binds on the results
#   my_args <- match.call()
#   tags2 <- ("search_tags2" %in% names(my_args))
#   if(tags2) {
#     tempa2 <- fred$tags.series(search_tags2)
#     tempa1 <- rbind(tempa1,tempa2)
#   }
#      tempb <- tempa1 %>%
#        filter(frequency==freq_var) %>%
#        # drops any that start with either of these two words
#        filter(!grepl(drop_title, title)) %>%
#        # keeps only those that match
#        filter(grepl(keep_title, title)) %>%
#       filter(grepl(series_id_gsubend, id))
#   fred_ces_msa_list1 <- tempb %>%
#     select(id,title) %>%
#     mutate(msa_code = gsub(series_id_gsubend, "", id))
#   return(fred_ces_msa_list1)
# }
# b <- do.call("testfun",temp_l)


####################################
#
# approach of defining list of arguments to do Fred pull by concept
#

# defines function that pulls data from FRED
# outputs the table of variables to pull and the resulting data
#' pull data from FRED
#'
#' @param series_id
#' @param ac_substr_1
#' @param ac_substr_2
#' @param var_substr_1
#' @param var_substr_2
#' @param freq
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pull_emp <- function(series_id, ac_substr_1, ac_substr_2, var_substr_1, var_substr_2, freq, ...) {
  print(series_id)
  print(ac_substr_2)
  # creates a dataframe containing results of a series search with certain columns added
  table_to_pull <- getFredTable_list_aranmod(...)
  # uses that to create a list of series to pull
  to_pull <- paste(table_to_pull$msa_code, series_id, sep="")
  b <- getFredTable(to_pull, frequency=freq, aggregation_method="avg", ...) %>%
    data.frame(date=time(.), .)  %>%
    gather(area_code_var, value, -date, na.rm = FALSE) %>%
    mutate(area_code = str_sub(area_code_var, ac_substr_1, ac_substr_2)) %>%
    mutate(var = str_sub(area_code_var, var_substr_1, var_substr_2)) %>%
    select(date, area_code, var, value)
  list(table_to_pull=table_to_pull, output=b)
}


# defines function that applies a given function, using a list of inputs, and
# then takes the two resulting outs and puts them into a list
#' apply a function given a list of inputs, extract results
#'
#' @param pull_fun
#' @param list_input
#'
#' @return
#' @export
#'
#' @examples
pull_list <- function(pull_fun, list_input) {
  # do.call basically passes the elements of its second argument (a list)
  # as arguments to a function you specify
  c <- do.call(pull_fun,list_input)
  table_to_pull_l <- c[[1]]
  output_l <- c[[2]]
  list(table_to_pull_l=table_to_pull_l, output_l=output_l)
}


