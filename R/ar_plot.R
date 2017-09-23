
#' adds the title
#'
#' @param plot
#' @param grtitle
#' @param footnote
#'
#' @return
#' @export
#'
#' @examples
plot_title_1=function(plot, grtitle, footnote){
  # in the following row, I had added ?ncol=1, nrow=1? in the
  # fred functions script. Wasn?t in lodfor. Not sure what impact
  # it will have, but I added it to the combined docoument.
  grobframe <- arrangeGrob(plot, ncol=1, nrow=1,
                           main = textGrob(grtitle, x=0, hjust=0, vjust=0.6,
                                           gp = gpar(fontsize=16, fontface="bold")),
                           sub = textGrob(footnote, x=0, hjust=0, vjust=0.1,
                                          gp = gpar(fontface="plain", fontsize=7)))
  grid.newpage() # basic command to create a new page of output
  grid.draw(grobframe)
  # these worked but didn't improve things much I thought
  #ggsave(grobframe,file="whatever.png",compression="lzw",height=5.5,width=9,dpi=1000,units="in")
  #ggsave(grobframe,file="whatever.png",height=5.5,width=9,dpi=900,units="in")
}

#' another approach to add title
#'
#' @param plot
#' @param grtitle
#' @param footnote
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
plot_title_2=function(plot, grtitle, footnote, filename){
  grobframe <- arrangeGrob(plot, ncol=1, nrow=1,
                           main = textGrob(grtitle, x=0, hjust=0, vjust=0.6,
                                           gp = gpar(fontsize=16, fontface="bold")),
                           sub = textGrob(footnote, x=0, hjust=0, vjust=0.1,
                                          gp = gpar(fontface="plain", fontsize=7)))
  grid.newpage() # basic command to create a new page of output
  grid.draw(grobframe)
  # these worked but didn't improve things much I thought
  #ggsave(grobframe,file="whatever.tiff",compression="lzw",height=5.5,width=9,dpi=1000,units="in")
  #ggsave(grobframe,file="whatever.emf",height=5.7,width=9,dpi=800,units="in")
  ggsave(grobframe,file=filename,height=5.7,width=9,dpi=800,units="in")
}


#' another approach to add a title
#'
#' in the following I had added ?filename? in the fred version.
#' I included in the combined document. Not sure what impact that will have
#'
#' @param plot
#' @param grtitle
#' @param footnote
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
plot_title_3=function(plot, grtitle, footnote, filename){
  # in the fred version I had dropped heights and replaced it with ncol and row
  # I?ve incorporated that change in the combined document. Not sure what
  # impact that will have. Also replaced ?sub? with ?bottom?
  grobframe <- arrangeGrob(plot, ncol=1, nrow=1,
                           top = textGrob(grtitle, x=0, hjust=0, vjust=0.6,
                                          gp = gpar(fontsize=16, fontface="bold")),
                           bottom = textGrob(footnote, x=0, hjust=0, vjust=0.1,
                                             gp = gpar(fontface="plain", fontsize=7)))
  grid.newpage() # basic command to create a new page of output
  grid.draw(grobframe)
  # these worked but didn't improve things much I thought

  #ggsave(grobframe,file="whatever.png",compression="lzw",height=5.5,width=9,dpi=1000,units="in")
  #ggsave(grobframe,file="whatever.png",height=5.5,width=9,dpi=900,units="in")
  ggsave(grobframe,file=filename,height=5.7,width=9,dpi=800,units="in")
}

#' Plotting function
#'
#' This is the plotting function that I was using for some time in the US overview graphs. I had previously had some
#' troubling moving it to the package. But when I cleaned up how I was referring to paths, it seemed
#' to work. So here it is.
#'
#' @param plot
#' @param grtitle
#' @param subtitle
#' @param footnote
#' @param filename
#' @param saveheight
#' @param savewidth
#'
#' @return
#' @export
#'
#' @examples
plot_title_5=function(plot, grtitle, subtitle, footnote, filename, saveheight=5.7,
                      savewidth=8.5){
  # create a list of grobs in order
  title_grob <- textGrob(grtitle, x=0, hjust=0, vjust=0.6,
                         gp = gpar(fontsize=16, fontface="bold"))
  subtitle_grob <- textGrob(subtitle, x=0, hjust=0, gp = gpar(fontsize=10, col="grey20"))
  footnote_grob <- textGrob(footnote, x=0, hjust=0, vjust=0.1,
                            gp = gpar(fontface="plain", fontsize=7, col="grey20"))
  groblist <- list(title_grob, subtitle_grob, plot, footnote_grob)

  grobframe <- arrangeGrob(ncol=1, nrow=4,
                           # height of each row defined using npc, where npc means
                           # normalised parent coordinates - basically analogous to
                           # a proportion of the plot area, so values range from 0 to 1
                           heights=unit(c(.1, .05, .75, .1), "npc"), grobs=groblist)
  grid.newpage() # basic command to create a new page of output
  grid.draw(grobframe)
  ggsave(grobframe,file=filename,height=saveheight,width=savewidth,dpi=400,units="in")
}

#' combines two plots together (e.g. employment and GDP)
#'
#' to get this function to work I needed to modify ggsave using the following line
#' this is because I've used ggplot_gtable, and so the pieces aren't ggplot plot
#' elements, and that's what's expected in ggsave. Based on:
#' http://stackoverflow.com/questions/18406991/saving-a-graph-with-ggsave-after-using-ggplot-build-and-ggplot-gtable
#' this should no longer be necessary, so I've commented it out
#' ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2]
#'
#'
#' @param p1
#' @param p2
#' @param grtitle
#' @param footnote
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
plot_title_two1=function(p1, p2, grtitle, footnote, filename){
  gp1<- ggplot_gtable(ggplot_build(p1))
  gp2<- ggplot_gtable(ggplot_build(p2))
  maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
  gp1$widths[2:3] <- maxWidth
  gp2$widths[2:3] <- maxWidth

  grobframe <- arrangeGrob(gp2, gp1, ncol=1, nrow=2,
                           main = textGrob(grtitle, x=0, hjust=0, vjust=0.6,
                                           gp = gpar(fontsize=16, fontface="bold")),
                           sub = textGrob(footnote, x=0, hjust=0, vjust=0.1,
                                          gp = gpar(fontface="plain", fontsize=7)))
  grid.newpage() # basic command to create a new page of output
  grid.draw(grobframe)
  ggsave(grobframe,file=filename,height=5.7,width=9,dpi=800,units="in")
}
