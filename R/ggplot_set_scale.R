#' set format of scale_y_continuous
#'
#' Created to be used in automated creation of graphs when the format of the y-axis (horizontal
#' axis when used in bar graph after coord_flip()) needs to be set based on a variable
#' (e.g. graphing concepts such as occupancy and revpar in the same process.
#'
#' @param plot1
#' @param set_axis_type either "dollar" or "percent"

#'
#' @return
#' @export
#'
#' @examples
#'

#'
#'
#'
ggplot_set_scale_y_con <- function(plot1, set_axis_type) {

  # handle percent case
  if (set_axis_type=="percent") {
    plot1 <- plot1 +
      ggplot2::scale_y_continuous(labels=scales::percent)
  }

  # handle dollar case
  if (set_axis_type=="dollar") {
    plot1 <- plot1 +
      ggplot2::scale_y_continuous(labels=scales::dollar)
  }
  plot1
}
