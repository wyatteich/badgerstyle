.badger_png_device <- function(device, dpi) {
  device <- match.arg(device, c("png", "ragg", "auto"))
  if (length(dpi) != 1L || !is.numeric(dpi) || !is.finite(dpi) || dpi <= 0) {
    stop("`dpi` must be a positive finite numeric scalar.", call. = FALSE)
  }
  if (device == "auto") {
    device <- if (requireNamespace("ragg", quietly = TRUE)) "ragg" else "png"
  }
  if (device == "ragg" && !requireNamespace("ragg", quietly = TRUE)) {
    stop("Install the `ragg` package to use `device = \"ragg\"`.", call. = FALSE)
  }
  device
}

.badger_render_png <- function(filename, width, height, res, draw, device = "png") {
  device <- .badger_png_device(device, res)
  open_png <- if (device == "ragg") ragg::agg_png else grDevices::png
  open_png(
    filename = filename,
    width = width,
    height = height,
    units = "in",
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
