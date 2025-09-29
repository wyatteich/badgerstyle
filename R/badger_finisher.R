  #' Finisher
  #'
  #' This function places the finishing touches on a Badger graph.
  #' @param plot The ggplot2 object that the finisher will "spruce up"
  #' @param head The headline for the plot
  #' @param source The source attribution of the plot.
  #' @param logo_ref The file reference for the logo.
  #' @param filename The desired file name for the plot
  #' @param aspect The desired aspect ratio for the plot.


badger_finisher <- function(plot,
                            head = "Insert Headline",
                            source = "Insert Source",
                            logo_ref = "C:/Users/Wyatt Eichholz/OneDrive - Badger Institute/R Reference/Badger-Institute-Icon.png",
                            filename = "plot.png",
                            aspect = c("default", "1col", "2col", "web", "ppt", "custom"),
                            border = T,
                            height = 5, width  = 9.55) {


  # applies external formatting to grid, including the border, title, and source caption.
  extrafont::loadfonts(device = "win", quiet = T)

  titlefont <- "Franklin Gothic Demi Cond"
  font <- "Franklin Gothic Medium Cond"
  text_color <- "#222222"

  # insert logo
  #plot <- cowplot::ggdraw(plot) +
    #cowplot::draw_image(logo_ref, x = 1, y = 0, hjust = 1, vjust = 1, width = 0.05, height = 0.05)

  img <- png::readPNG(logo_ref)

  aspect_ratios <- list(
    default = list(h = 5, w = 9.55),
    `1col` = list(h = 3.84, w = 3.79),
    `2col` = list(h = 3.84, w = 7.83),
    web = list(h = 4, w = 5.8),
    ppt = list(h = 3.9375, w = 7),
    custom = list(h = height, w = width)
  )

  h <- aspect_ratios[[aspect]]$h
  w <- aspect_ratios[[aspect]]$w



  grDevices::png(filename = filename,
      width = w,
      height = h,
      unit = "in",
      res = 864)
  grid::grid.newpage()

  gridExtra::grid.arrange(
    plot +
      ggplot2::labs(
        title="",
        caption=""),

    top = grid::textGrob(
      label = head,
      hjust=0,
      x=0.02,
      y = 0.005,

      gp=grid::gpar(
        fontfamily = titlefont,
        fontsize = 16
      )
    ),

    bottom =  gridExtra::arrangeGrob(
      grid::textGrob(
        label = source,
        hjust=0,
        x=0.025,
        y = 1.1,

        gp=grid::gpar(
          fontfamily=font,
          fontsize = 8
        )
      ),

      grid::rasterGrob(
        img,
        x = 0.975, hjust = 1,
        y = 0.6, vjust = 0,
        interpolate = TRUE,
        width = unit(0.2, units = "in"),
        height = unit(0.2, units = "in")
      ),

      widths = unit(c(2,1), "null"),
      ncol = 2
    )
  )

  if (border == T) {
    grid::grid.rect(.5, .5,
                    width=unit(1,"npc"),
                    height=unit(1,"npc"),

                    gp=grid::gpar(lwd=3, fill=NA, col="#747F81"))
  }



  grDevices::dev.off()

  #return(final)
}
