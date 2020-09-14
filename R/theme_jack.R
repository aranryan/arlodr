
#' Set theme example
#'
#' @param base_size
#' @param base_family
#'
#' @return
#' @export
#'
#' @examples
theme_jack <- function (base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(                                  #text=element_text(family="Lato Light", size=14),
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.minor.y=element_blank(),
      panel.grid.major.y=element_line(colour="#ECECEC", size=0.5, linetype=1),
      axis.ticks.y=element_blank(),
      panel.background=element_blank(),
      legend.title=element_blank(),
      legend.key=element_rect(fill="white", colour = "white"),
      legend.key.size=unit(1.5, "cm"),
      legend.text=element_text(size=16),
      axis.title=element_text(size=10),
      axis.text=element_text(color="black",size=13)
    )
}
