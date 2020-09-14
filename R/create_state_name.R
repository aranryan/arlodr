

#' Create state name dataframe
#'
#' Creates a dataframe with a row for each state. Provides:
#' cen_region Census Region
#' cen_division Census Division
#' state_name State name written out
#' state_abb Two-letter state code in upper case
#'
#' The dataframe has 51 rows because DC has been appended.
#' Also, the Middle Atlantic Division is referred to as Mid-Atlantic, and
#' Midwest is used to replace the former name of the North Central Census
#' region.
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' state_name <- create_state_name_df()
#'
create_state_name_df <- function(x) {
  # load the state data set in base r
  data(state)
  state.division <- as.character(state.division)
  # appends South Atlantic to the end so that it can be used
  # for DC
  state.division <- c(state.division, "South Atlantic")
  # rename the Middle Atlantic as Mid-Atlantic
  state.division <- gsub("Middle Atlantic", "Mid-Atlantic", state.division)
  # similar step for regions
  state.region <- as.character(state.region)
  # rename the North Central region as Midwest (Census made change in 1984)
  state.region <- gsub("North Central", "Midwest", state.region)
  state.region <- c(state.region, "South")
  state.region
  # appends DC to the list of state names
  state.name <- c(state.name, "District of Columbia")
  # appends DC to the list of state two-letter abbreviations
  state.abb <- c(state.abb, "DC")

  state_name <- data_frame(state.name, state.division, state.region, state.abb) %>%
    rename(state_name = state.name,
           cen_region = state.region,
           cen_division = state.division,
           state_abb = state.abb) %>%
    # define region as a factor with levels in a defined order
    dplyr::mutate(cen_region = factor(cen_region, levels=c("Northeast", "Midwest", "South", "West"))) %>%
    # define division as a factor
    dplyr::mutate(cen_division = factor(cen_division, levels=c("New England",
                                                "Mid-Atlantic",
                                                "East North Central",
                                                "West North Central",
                                                "South Atlantic",
                                                "East South Central",
                                                "West South Central",
                                                "Mountain",
                                                "Pacific"))) %>%
    # creates factor defaulting to alphabetical order for the levels
    mutate(state_name = factor(state_name)) %>%
    select(cen_region, cen_division, state_name, state_abb) %>%
    # put in order
    arrange(cen_region, cen_division, state_name)
}
