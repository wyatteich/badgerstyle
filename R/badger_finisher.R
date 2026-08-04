#' Save a publication-ready Badger graphic
#'
#' Renders a ggplot to a high-resolution PNG with a separate headline, source
#' line, Badger Institute icon, and optional border. The output dimensions can
#' use a publication preset or custom dimensions.
#'
#' @param plot A ggplot object to render.
#' @param head Character string used as the headline above the plot.
#' @param source Character string used as the source line below the plot.
#' @param logo_ref Path to the PNG logo. Defaults to the Badger Institute icon
#'   installed with the package.
#' @param filename Output path for the PNG file.
#' @param aspect One of `"default"`, `"1col"`, `"2col"`, `"web"`, `"ppt"`,
#'   or `"custom"`. The preset determines the output dimensions in inches.
#' @param border Logical; draw a gray border around the finished graphic.
#' @param height,width Custom output dimensions in inches. Used only when
#'   `aspect = "custom"`.
#' @param register_fonts Logical; register the Badger fonts before rendering.
#'   Registration is cached for the remainder of the R session.
#' @param title_family,text_family Font families used for the headline and
#'   source line. The defaults use the standard Badger Institute fonts.
#'
#' @return Invisibly returns the result of closing the PNG graphics device.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + badger_style()
#' badger_finisher(
#'   p,
#'   head = "Fuel economy falls as vehicle weight rises",
#'   source = "Source: Motor Trend",
#'   filename = "fuel-economy.png",
#'   aspect = "web"
#' )
#' }
#'
#' @seealso [badger_style()], [badger_publish()]
#' @export


badger_finisher <- function(plot,
                            head = "Insert Headline",
                            source = "Insert Source",
                            logo_ref = system.file("img", "Badger-Institute-Icon.png", package = "badgerstyle"),
                            filename = "plot.png",
                            aspect = c("default", "1col", "2col", "web", "ppt", "custom"),
                            border = TRUE,
                            height = 5, width  = 9.55,
                            register_fonts = TRUE,
                            title_family = "Franklin Gothic Demi Cond",
                            text_family = "Franklin Gothic Medium Cond") {

  aspect <- match.arg(aspect)
  .badger_scalar_logical(border, "border")
  .badger_scalar_logical(register_fonts, "register_fonts")
  if (length(height) != 1L || !is.numeric(height) || !is.finite(height) || height <= 0) {
    stop("`height` must be a positive finite numeric scalar.", call. = FALSE)
  }
  if (length(width) != 1L || !is.numeric(width) || !is.finite(width) || width <= 0) {
    stop("`width` must be a positive finite numeric scalar.", call. = FALSE)
  }
  for (argument in c("title_family", "text_family")) {
    value <- get(argument)
    if (length(value) != 1L || !is.character(value) || is.na(value)) {
      stop("`", argument, "` must be a single character string.", call. = FALSE)
    }
  }

  if (register_fonts) badger_register_fonts()

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



  .badger_render_png(filename, w, h, 864, draw = function() {
    grid::grid.newpage()

    gridExtra::grid.arrange(
      plot + ggplot2::labs(title = "", caption = ""),
      top = grid::textGrob(
        label = head,
        hjust = 0,
        x = 0.02,
        y = 0.005,
        gp = grid::gpar(fontfamily = title_family, fontsize = 16)
      ),
      bottom = gridExtra::arrangeGrob(
        grid::textGrob(
          label = source,
          hjust = 0,
          x = 0.025,
          y = 1.1,
          gp = grid::gpar(fontfamily = text_family, fontsize = 8)
        ),
        grid::rasterGrob(
          img,
          x = 0.975,
          hjust = 1,
          y = 0.6,
          vjust = 0,
          interpolate = TRUE,
          width = grid::unit(0.2, units = "in"),
          height = grid::unit(0.2, units = "in")
        ),
        widths = grid::unit(c(2, 1), "null"),
        ncol = 2
      )
    )

    if (border) {
      grid::grid.rect(
        0.5,
        0.5,
        width = grid::unit(1, "npc"),
        height = grid::unit(1, "npc"),
        gp = grid::gpar(lwd = 3, fill = NA, col = "#747F81")
      )
    }
  })

}
