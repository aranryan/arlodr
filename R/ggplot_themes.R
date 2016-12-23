#' defined theme for use in creating plots
#'
#' @param base_size
#' @param base_family
#'
#' @return
#' @export
#'
#' @examples
theme_ts1 <- function (base_size = 14, base_family = "Arial") {
  theme_classic(base_size = base_size) %+replace%
    theme(
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.minor.y=element_blank(),
      axis.ticks.y=element_line(size=.5, colour = "grey70"),
      axis.ticks.x=element_line(size=.5, colour = "grey70"),
      panel.background=element_blank(),
      legend.title=element_blank(),
      legend.key=element_rect(fill="white", colour = "white"),
      legend.key.size=unit(1, "cm"),
      legend.text=element_text(size=rel(1)),
      legend.position = "none",
      legend.justification = "right",
      axis.line.x = element_line(size=.2, colour = "grey70"),
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text=element_text(color="grey20",size=rel(.7)),
      # following seems to adjust y-axis title
      plot.title=element_text(size=base_size * .8,face="plain", hjust=0,
                              vjust=1),
      # Element plot.margin is a four-element vector associated with the
      # margins to be placed outside the graphics region going clockwise from the top.
      # Top, right, bottom, left
      plot.margin = unit(c(.01, 0, .01, 0), "npc")
    )
}
