#' Badger Style
#'
#' This creates a ggplot theme object that applies Badger Institute's style.

badger_style <- function() {
  extrafont::loadfonts(device = "win", quiet = T)

  titlefont <- "Franklin Gothic Demi Cond"
  font <- "Franklin Gothic Medium Cond"
  text_color <- "#222222"

  badger_theme <- ggplot2::theme(

    # title
    plot.title = ggplot2::element_text(
      family = titlefont,
      size = 16,
      #face = "bold",
      color = text_color,
      hjust = 0,
      margin = ggplot2::margin(0, 0, -8, 0)
    ),

    # subtitle
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 14,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),

    # caption
    plot.caption = ggplot2::element_blank(),

    # margin

    plot.margin = ggplot2::margin(t = 15, l = 10, r = 10, b = 15),

    # legend
    legend.position = "bottom",
    legend.margin = ggplot2::margin(t = -5, r = 0, l = 0, b = 0),
    #legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.key.size = unit(0.4, "cm"),
    legend.text = ggplot2::element_text(
      family = font,
      size = 10,
      color = text_color,
      hjust = 0
    ),

    # axis
    axis.title = ggplot2::element_text(
      family = font,
      size = 10,
      color = text_color
    ),
    axis.title.x = ggplot2::element_text(
      vjust = 1,
      margin = ggplot2::margin(7, 0, 0, 0)
    ),
    axis.title.y = ggplot2::element_text(
      vjust = 0,
      margin = ggplot2::margin(0, 5, 0, 0)
    ),
    axis.text = ggplot2::element_text(
      family = font,
      size = 9,
      color = text_color
    ),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t=3, b = 5)
    ),
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(5, l = 5)
    ),
    #axis.ticks.length = ggplot2::element_blank(),
    axis.ticks.length = ggplot2::unit(0.05, "in"),
    axis.ticks.x.bottom = ggplot2::element_line(
      linewidth = 0.3,
      arrow = grid::arrow(
        angle = 40,
        length = unit(0.07, "in"),
        ends = "last",
        type = "closed")),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),

    # panel
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#747F81"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),

    # strip
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 10, hjust = 0),
  )


  clip <- coord_cartesian(clip = "off")

  list(badger_theme, clip)
}

