#' Badger Publish
#'
#' This function saves the completed plot as a high quality PNG
#'
#' @param filename The desired filename for the image
#' @param plot The plot object to be saved
#' @param columns Whether the size parameters should be set for a one column graphic, or two

badger_publish <- function(filename = "plot.png", plot, aspect = c("1col", "2col", "web")) {
  # will format the size of the graphic according to publisher size specifications

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


  png(filename = filename,
      width = w,
      height = h,
      unit = "in",
      res = 864)
  grid.newpage()
  grid.draw(plot)
  dev.off()
}
