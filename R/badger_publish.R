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
#' @inheritParams badger_finisher
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

badger_publish <- function(filename = "plot.png", plot, aspect = c("1col", "2col", "web"),
                           dpi = 864, device = c("png", "ragg", "auto")) {
  # will format the size of the graphic according to publisher size specifications

  aspect <- match.arg(aspect)
  device <- .badger_png_device(match.arg(device), dpi)

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


  .badger_render_png(filename, w, h, dpi, device = device, draw = function() {
    grid::grid.newpage()
    grid::grid.draw(plot)
  })
}
