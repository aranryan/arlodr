
#' Convert str data from Excel with merged column headers to tidy csv
#'
#'Function that can import the US or WW data file from STR.
#' This has to deal with the fact that the file has merged cells for the market names.
#'
#' @param input_file_name Name of the input file.
#' @param output_file_name Name of the output file.
#'
#' @param input_path
#'
#' @return
#' @export
#'
#' @examples
#'
#' arlodr::convert_str_xlmerged(
#' input_file_name = "TourismEconomicsUS_201708.xls",
#' output_file_name = "strtidy_us_201708.csv",
#' input_path = c("input_data/str_data_restrict/"))
#'
convert_str_xlmerged <- function(input_file_name, output_file_name, input_path){

  temp_data <- readxl::read_excel(path = paste0(input_path, input_file_name), sheet = 2, cell_rows(2:500))
  temp_row1 <- readxl::read_excel(path = paste0(input_path, input_file_name), sheet = 2, cell_rows(1:1), col_names = FALSE)

  # fix basic column name
  colnames(temp_data) <- gsub("X__1", "date", colnames(temp_data))

  #########
  #
  # create a list of markets based on the first row of the excel file

  # create a vector of the first row
  a <- as.character(as.vector(temp_row1[1,]))

  # remove values that are character "NA"
  mkt_text <- a [! a %in% c("NA")]

  # create numeric vector
  num_vec <- 0:(length(mkt_text)-1)

  # create dataframe with market names
  mkt_names <- data_frame(mkt = num_vec,
                          geo = mkt_text)

  ########
  #
  # organize raw data
  temp_data_1 <- temp_data %>%
    gather(variable, value, -date) %>%
    filter(!(is.na(value))) %>%
    # checks to see if the variable ends with a letter, in other words
    # it is the beginning, and a number hasn't been appended.
    # So we want to append an underscore and 0.
    mutate(variable_new = ifelse(grepl("[a-zA-Z]$", variable),
                                 paste0(variable, "__0"),
                                 variable)) %>%
    select(-variable) %>%
    # separate into a variable and a market
    separate(col = variable_new, into=c("variable", "mkt"), sep = "__", extra="merge", remove=TRUE) %>%
    # convert variables
    mutate(variable = ifelse(variable == "Demand", "demt", variable)) %>%
    mutate(variable = ifelse(variable == "Supply", "supt", variable)) %>%
    mutate(variable = ifelse(variable == "Revenue", "rmrevt", variable)) %>%
    mutate(mkt = as.integer(mkt)) %>%
    # format date
    mutate(date = as.Date(as.yearmon(format(date, nsmall =2), "%Y%m"))) %>%
    select(date, variable, mkt, value)

  #######
  #
  # join on the market names

  temp_data_2 <- temp_data_1 %>%
    left_join(mkt_names, by=("mkt")) %>%
    select(date, variable, geo, value, -mkt)

  str_data <- temp_data_2

  #######
  #
  # Write output
  write_csv(str_data, output_file_name)
}
