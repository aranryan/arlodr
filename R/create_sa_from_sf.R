

#' Use seasonal factors to create monthly or quarterl seasonally adjusted data
#'
#' my thought process on this function had originally been that it would start
#' with a dataframe that had a single underscore between the segment code and
#' the variable code. So all segment and geographic info would be in the segment
#' code. But then I got into the Host work, and realized that it was potentially
#' useful to have additional segment, geography, country information in the
#' mneomonic, separated by underscores. But I couldn't figure out how to write a
#' regular express than would just break of the lodging variable concept. What I
#' could do was break up the other stuff until I was left with the lodging
#' concept. But that pointed to the idea of staying flexible on the set up of
#' the dataframe going into this function. So it is more generalized. Also, I
#' realized that the monthly and quarterly were doing the same thing, so I
#' combined them. I left the monthly and quarterly ones there, but I could get
#' rid of them once I bring the US lodfor process onto the same footing.

#' @param df
#'
#' @return
#' @export
#'
#' @examples
create_sa_str <- function(df){

  # create either monthly or quarterly sa from seasonal factors
  # requires as an input a dataframe with lodging variables separate
  # any number of geographic or segment columns is fine
  df <- df %>%
    mutate(occ_sa = occ / occ_sf) %>%
    mutate(revpar_sa = revpar / revpar_sf) %>%
    mutate(adr_sa = adr / adr_sf) %>%
    mutate(demd_sa = demd / demd_sf) %>%
    mutate(supd_sa = supd / supd_sf) %>%
    mutate(demar_sa = demd_sa * 365) # creates demand at an annual rate
  return(df)
}

#' use seasonal factors to create monthly seasonally adjusted data
#'
#' function takes a monthly data frame with monthly data and seasonal factors
#' and creates monthly sa
#'
#' @param str_m
#'
#' @return
#' @export
#'
#' @examples
create_sa_str_m <- function(str_m){

  # following converts to a tidy format, uses seasonal factors to calculate sa
  # series, then converts back to a wide dataframe
  str_m <- str_m %>%
    # creates column called segvar that contains the column names, and one next to
    # it with the values, dropping the time column
    tidyr::gather(segvar, value, -date, na.rm = FALSE) %>%
    # in the following the ^ means anything not in the list
    # with the list being all characters and numbers
    # so it separates segvar into two colums using sep
    # it separates on the _, as long as it's not followed by sf
    # the not followed piece uses a Negative Lookahead from
    # http://www.regular-expressions.info/lookaround.html
    tidyr::separate(segvar, c("seg", "variable"), sep = "_(?!sf)") %>%
    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    tidyr::spread(variable,value) %>%
    mutate(occ_sa = occ / occ_sf) %>%
    mutate(revpar_sa = revpar / revpar_sf) %>%
    mutate(adr_sa = adr / adr_sf) %>%
    mutate(demd_sa = demd / demd_sf) %>%
    mutate(supd_sa = supd / supd_sf) %>%
    mutate(demar_sa = demd_sa * 365) %>% # creates demand at an annual rate
    # puts it back into a wide data frame, with one column for each series
    # days is a series for each segment/market\
    reshape2::melt(id=c("date","seg"), na.rm=FALSE) %>%
    mutate(variable = paste(seg, "_", variable, sep='')) %>%
    select(-seg) %>%
    tidyr::spread(variable,value)
  # if instead I had wanted an xts object, I could have done
  #read.zoo(split = 2) %>%
  #xts()
  return(str_m)
}


#' use seasonal factors to create monthly seasonally adjusted data
#'
#' function takes a monthly data frame with monthly data and seasonal factors
#' and creates monthly sa

#' @param str_q
#'
#' @return
#' @export
#'
#' @examples
create_sa_str_q <- function(str_q){

  # create quarterly sa from seasonal factors
  # following converts to a tidy format, uses seasonal factors to calculate sa
  # series, then converts back to a wide dataframe
  str_q <- str_q %>%
    # creates column called segvar that contains the column names, and one next to
    # it with the values, dropping the time column
    tidyr::gather(segvar, value, -date, na.rm = FALSE) %>%
    # in the following the ^ means anything not in the list
    # with the list being all characters and numbers
    # so it separates segvar into two colums using sep
    # it separates on the _, as long as it's not followed by sf
    # the not followed piece uses a Negative Lookahead from
    # http://www.regular-expressions.info/lookaround.html
    tidyr::separate(segvar, c("seg", "variable"), sep = "_(?!sf)") %>%
    # keeps seg as a column and spreads variable into multiple columns containing
    # the values
    tidyr::spread(variable,value) %>%
    mutate(occ_sa = occ / occ_sf) %>%
    mutate(revpar_sa = revpar / revpar_sf) %>%
    mutate(adr_sa = adr / adr_sf) %>%
    mutate(demd_sa = demd / demd_sf) %>%
    mutate(supd_sa = supd / supd_sf) %>%
    mutate(demar_sa = demd_sa * 365) %>% # creates demand at an annual rate
    # puts it back into a wide data frame, with one column for each series
    # days is a series for each segment/market\
    reshape2::melt(id=c("date","seg"), na.rm=FALSE) %>%
    mutate(variable = paste(seg, "_", variable, sep='')) %>%
    select(-seg) %>%
    tidyr::spread(variable,value)
  # if instead I had wanted an xts object, I could have done
  #read.zoo(split = 2) %>%
  #xts()
  return(str_q)
}

