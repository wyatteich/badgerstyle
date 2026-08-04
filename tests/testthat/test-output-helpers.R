test_that("badger_style can skip font registration", {
  style <- badger_style(register_fonts = FALSE)
  expect_type(style, "list")
  expect_length(style, 2L)
  expect_s3_class(style[[1L]], "theme")
  expect_s3_class(style[[2L]], "CoordCartesian")
})

test_that("PNG rendering closes its device after success and failure", {
  success_path <- tempfile(fileext = ".png")
  failure_path <- tempfile(fileext = ".png")
  device_before <- grDevices::dev.cur()

  badgerstyle:::.badger_render_png(success_path, 2, 2, 72, function() {
    graphics::plot.new()
  })
  expect_true(file.exists(success_path))
  expect_gt(file.info(success_path)$size, 0)
  expect_equal(grDevices::dev.cur(), device_before)

  expect_error(
    badgerstyle:::.badger_render_png(failure_path, 2, 2, 72, function() {
      stop("drawing failed")
    }),
    "drawing failed"
  )
  expect_equal(grDevices::dev.cur(), device_before)
})

test_that("badger_publish writes a PNG", {
  path <- tempfile(fileext = ".png")
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  badger_publish(path, plot, aspect = "web")
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})

test_that("badger_finisher writes a composed PNG", {
  path <- tempfile(fileext = ".png")
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  badger_finisher(
    plot,
    head = "Test headline",
    source = "Source: test",
    filename = path,
    aspect = "custom",
    height = 2,
    width = 3,
    register_fonts = FALSE,
    title_family = "sans",
    text_family = "sans"
  )
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})

test_that("write_badger_table writes a readable workbook", {
  path <- tempfile(fileext = ".xlsx")
  data <- data.frame(
    year = 2024:2025,
    rate = c(0.03, 0.04),
    population = c(1000, 1100)
  )

  workbook <- write_badger_table(
    data,
    path,
    col_types = c(year = "year", rate = "percent", population = "pop"),
    source = "Source: test"
  )
  expect_true(file.exists(path))
  expect_s3_class(workbook, "wbWorkbook")
})
