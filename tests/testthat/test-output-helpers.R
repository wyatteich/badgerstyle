test_that("badger_style can skip font registration", {
  style <- badger_style(register_fonts = FALSE)
  expect_type(style, "list")
  expect_length(style, 2L)
  expect_s3_class(style[[1L]], "theme")
  expect_s3_class(style[[2L]], "CoordCartesian")
})

test_that("badger_style registers Badger fonts with the Windows device", {
  skip_if(.Platform$OS.type != "windows")

  badger_style()
  registered <- names(grDevices::windowsFonts())

  expect_true("Franklin Gothic Medium Cond" %in% registered)
  expect_true("Franklin Gothic Demi Cond" %in% registered)

  path <- tempfile(fileext = ".png")
  expect_no_warning(
    badgerstyle:::.badger_render_png(path, 3, 2, 150, function() {
      grid::grid.newpage()
      grid::grid.text(
        "Badger body",
        gp = grid::gpar(fontfamily = "Franklin Gothic Medium Cond")
      )
      grid::grid.text(
        "Badger headline",
        y = 0.3,
        gp = grid::gpar(fontfamily = "Franklin Gothic Demi Cond")
      )
    })
  )
  expect_gt(file.info(path)$size, 0)
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

test_that("badger_finisher defaults reproduce the original layout exactly", {
  current_path <- tempfile(fileext = ".png")
  legacy_path <- tempfile(fileext = ".png")
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Title replaced by finisher")
  logo_ref <- system.file(
    "img",
    "Badger-Institute-Icon.png",
    package = "badgerstyle"
  )
  headline <- "First headline line\nSecond headline line"
  source <- "Source: regression test"

  badger_finisher(
    plot,
    head = headline,
    source = source,
    logo_ref = logo_ref,
    filename = current_path,
    aspect = "custom",
    height = 1.5,
    width = 2,
    register_fonts = FALSE,
    title_family = "sans",
    text_family = "sans"
  )

  logo <- png::readPNG(logo_ref)
  badgerstyle:::.badger_render_png(legacy_path, 2, 1.5, 864, function() {
    grid::grid.newpage()
    gridExtra::grid.arrange(
      plot + ggplot2::labs(title = "", caption = ""),
      top = grid::textGrob(
        label = headline,
        hjust = 0,
        x = 0.02,
        y = 0.005,
        gp = grid::gpar(fontfamily = "sans", fontsize = 16)
      ),
      bottom = gridExtra::arrangeGrob(
        grid::textGrob(
          label = source,
          hjust = 0,
          x = 0.025,
          y = 1.1,
          gp = grid::gpar(fontfamily = "sans", fontsize = 8)
        ),
        grid::rasterGrob(
          logo,
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
    grid::grid.rect(
      0.5,
      0.5,
      width = grid::unit(1, "npc"),
      height = grid::unit(1, "npc"),
      gp = grid::gpar(lwd = 3, fill = NA, col = "#747F81")
    )
  })

  expect_equal(png::readPNG(current_path), png::readPNG(legacy_path))
})

test_that("badger_finisher supports multiline title and source spacing", {
  path <- tempfile(fileext = ".png")
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  expect_no_warning(
    badger_finisher(
      plot,
      head = "First title line\nSecond title line",
      source = "Source: first line\nSource: second line",
      filename = path,
      aspect = "custom",
      height = 3,
      width = 4,
      register_fonts = FALSE,
      title_family = "sans",
      text_family = "sans",
      title_lineheight = 0.85,
      source_lineheight = 0.9,
      title_plot_padding = 7,
      title_border_padding = 6
    )
  )
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)

  expect_error(
    badger_finisher(
      plot,
      filename = tempfile(fileext = ".png"),
      register_fonts = FALSE,
      title_lineheight = 0
    ),
    "title_lineheight"
  )
  expect_error(
    badger_finisher(
      plot,
      filename = tempfile(fileext = ".png"),
      register_fonts = FALSE,
      source_lineheight = NA_real_
    ),
    "source_lineheight"
  )
  expect_error(
    badger_finisher(
      plot,
      filename = tempfile(fileext = ".png"),
      register_fonts = FALSE,
      title_plot_padding = -1
    ),
    "title_plot_padding"
  )
  expect_error(
    badger_finisher(
      plot,
      filename = tempfile(fileext = ".png"),
      register_fonts = FALSE,
      title_border_padding = Inf
    ),
    "title_border_padding"
  )
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
