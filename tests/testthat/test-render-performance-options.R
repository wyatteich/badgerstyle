test_that("both PNG backends honor physical size and clean up devices", {
  for (backend in c("png", "ragg")) {
    if (backend == "ragg" && !requireNamespace("ragg", quietly = TRUE)) next
    path <- tempfile(fileext = ".png")
    before <- grDevices::dev.list()
    badgerstyle:::.badger_render_png(path, 3, 2, 100, function() {
      grid::grid.newpage()
      grid::grid.text("Headline")
    }, device = backend)
    expect_equal(dim(png::readPNG(path))[1:2], c(200L, 300L))
    expect_identical(grDevices::dev.list(), before)
    expect_error(
      badgerstyle:::.badger_render_png(path, 3, 2, 100, function() {
        stop("drawing failed")
      }, device = backend),
      "drawing failed"
    )
    expect_identical(grDevices::dev.list(), before)
    unlink(path)
  }
})

test_that("automatic selection falls back when ragg is unavailable", {
  original_require <- base::requireNamespace
  local_mocked_bindings(requireNamespace = function(package, ...) {
    if (identical(package, "ragg")) FALSE else original_require(package, ...)
  }, .package = "base")
  expect_identical(badgerstyle:::.badger_png_device("auto", 150), "png")
  expect_identical(badgerstyle:::.badger_png_device("png", 150), "png")
  expect_error(badgerstyle:::.badger_png_device("ragg", 150), "Install.*ragg")
})

test_that("automatic selection uses ragg when available", {
  skip_if_not_installed("ragg")
  expect_identical(badgerstyle:::.badger_png_device("auto", 150), "ragg")
})

test_that("publication helpers forward resolution and backend", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  for (backend in c("png", "ragg")) {
    if (backend == "ragg" && !requireNamespace("ragg", quietly = TRUE)) next
    path <- tempfile(fileext = ".png")
    for (custom_spacing in c(FALSE, TRUE)) {
      args <- list(
        plot, head = "First line\nSecond line", source = "Source: test",
        filename = path, aspect = "custom", width = 4, height = 3,
        dpi = 100, device = backend, register_fonts = FALSE,
        title_family = "sans", text_family = "sans"
      )
      if (custom_spacing) args$title_plot_padding <- 6
      expect_no_warning(do.call(badger_finisher, args))
      expect_equal(dim(png::readPNG(path))[1:2], c(300L, 400L))
    }
    badger_publish(path, plot, aspect = "web", dpi = 100, device = backend)
    expect_equal(dim(png::readPNG(path))[1:2], c(400L, 580L))
    unlink(path)
  }
})

test_that("invalid rendering options fail before opening a file", {
  plot <- ggplot2::ggplot()
  path <- tempfile(fileext = ".png")
  for (dpi in list(0, -1, NA_real_, Inf, "150", c(100, 150), NULL)) {
    expect_error(badger_finisher(plot, filename = path, dpi = dpi), "dpi")
    expect_error(badger_publish(path, plot, dpi = dpi), "dpi")
  }
  expect_error(badger_finisher(plot, filename = path, device = "other"), "arg")
  expect_error(badger_publish(path, plot, device = "other"), "arg")
  expect_false(file.exists(path))
})

test_that("logo cache reuses decoding and refreshes changed or alternate files", {
  path <- tempfile(fileext = ".png")
  other <- tempfile(fileext = ".png")
  on.exit(unlink(c(path, other)), add = TRUE)
  png::writePNG(array(0, c(2, 3, 3)), path)
  first <- badgerstyle:::.badger_read_logo(path)
  expect_s3_class(first, "nativeRaster")
  local_mocked_bindings(
    readPNG = function(...) stop("logo decoded again"), .package = "png"
  )
  expect_identical(badgerstyle:::.badger_read_logo(path), first)
  # A distinct timestamp avoids filesystem clock granularity in this test.
  png::writePNG(array(1, c(4, 5, 3)), path)
  Sys.setFileTime(path, Sys.time() + 5)
  expect_error(badgerstyle:::.badger_read_logo(path), "logo decoded again")
  png::writePNG(array(0, c(2, 3, 3)), other)
  expect_error(badgerstyle:::.badger_read_logo(other), "logo decoded again")
})

test_that("updated logo pixels are returned and missing logos are not cached", {
  path <- tempfile(fileext = ".png")
  png::writePNG(array(0, c(2, 3, 3)), path)
  first <- badgerstyle:::.badger_read_logo(path)
  png::writePNG(array(1, c(4, 5, 3)), path)
  Sys.setFileTime(path, Sys.time() + 5)
  second <- badgerstyle:::.badger_read_logo(path)
  expect_false(identical(first, second))
  expect_equal(second, png::readPNG(path, native = TRUE))
  unlink(path)
  expect_error(badgerstyle:::.badger_read_logo(path))
})
