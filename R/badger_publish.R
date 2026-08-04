#' Save a Badger graphic using a legacy size preset
#'
#' Saves a plot as a high-resolution PNG using one of the original publication
#' size presets. New code should generally use [badger_finisher()], which also
#' adds the headline, source line, logo, and optional border.
#'
#' @param filename Output path for the PNG file.
#' @param plot A ggplot or grid grob to render.
#' @param aspect One of `"1col"`, `"2col"`, or `"web"`, controlling the
#'   output dimensions in inches.
#'
#' @return Invisibly returns the result of closing the PNG graphics device.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' badger_publish("fuel-economy.png", p, aspect = "web")
#' }
#'
#' @seealso [badger_finisher()]
#' @export

badger_publish <- function(filename = "plot.png", plot, aspect = c("1col", "2col", "web")) {
  # will format the size of the graphic according to publisher size specifications

  aspect <- match.arg(aspect)

  if (aspect == "1col") {
    h <- 3.84
    w<-3.79
  } else if (aspect == "2col") {
    h<-3.84
    w<-7.83
  } else if (aspect == "web") {
    h <- 4
    w <- 5.8
  } else {
    h<-5
    w<-7
  }


  # ggplot2::ggsave(
  #   filename,
  #   plot,
  #   dpi = 600,
  #   height = h,
  #   width = w,
  #   unit = "in"
  # )


  .badger_render_png(filename, w, h, 864, draw = function() {
    grid::grid.newpage()
    grid::grid.draw(plot)
  })
}
