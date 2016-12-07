#' Create a vector of geographies to be run for various equations.
#'
#' @param in_area_sh
#' @param ex_area_sh
#' @param market_vec
#'
#' @return
#' @export
#'
#' @examples
create_to_do_vec <- function(in_area_sh, ex_area_sh, market_vec) {
  # Break apart into a vector of elements rather than a single string.
  # In other words, what comes in from Excel ends up being recognized
  # as a single string, this converts it to a vector.
  in_area_sh <- unlist(strsplit(in_area_sh, split=", "))

  # Set up vector of area_sh to include
  if(in_area_sh[1] == "all"){
    in_area_sh_vec <- market_vec
  } else {
    in_area_sh_vec <- in_area_sh
  }

  # set up vector of area_sh to exclude
  if(is.na(ex_area_sh)[[1]]  ){
    ex_area_sh_vec <- c("")
  } else {
    ex_area_sh <- unlist(strsplit(ex_area_sh, split=", "))
    ex_area_sh_vec <- ex_area_sh
  }

  # drop those that should be excluded
  vec <- in_area_sh_vec [! in_area_sh_vec %in% ex_area_sh_vec]

  # Only include those that will have data. This helps to address the fact that
  # we can filter the source data frame to the point where it only contains data
  # for certain area_sh's, so we we only want to set up to run the analysis for
  # situations that are going to work. Maybe I could have handled in a join step
  # in the processing, but this also worked here.
  vec <- vec[vec %in% market_vec]
}
