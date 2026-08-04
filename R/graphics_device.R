.badger_render_png <- function(filename, width, height, res, draw) {
  grDevices::png(
    filename = filename,
    width = width,
    height = height,
    unit = "in",
    res = res
  )
  device <- grDevices::dev.cur()
  closed <- FALSE
  on.exit({
    open_devices <- grDevices::dev.list()
    if (!closed && !is.null(open_devices) && device %in% open_devices) {
      grDevices::dev.off(which = device)
    }
  }, add = TRUE)

  draw()
  result <- grDevices::dev.off(which = device)
  closed <- TRUE
  invisible(result)
}
