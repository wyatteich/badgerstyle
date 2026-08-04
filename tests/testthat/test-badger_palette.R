test_that("canonical palette matches established Badger colors", {
  expect_equal(
    badger_palette,
    c(
      badred = "#ED0000",
      badblue = "#313469",
      badgreen = "#008610",
      badgold = "#F69800",
      badliblue = "#99CCFF",
      badpurple = "#7439C3"
    )
  )
  expect_identical(badred, "#ED0000")
  expect_identical(badblue, "#313469")
  expect_identical(badlightblue, badliblue)
})

test_that("badger_colors accepts long and short palette names", {
  expect_equal(
    badger_colors("red", "badblue", "light blue"),
    c(badred = "#ED0000", badblue = "#313469", badliblue = "#99CCFF")
  )
  expect_equal(badger_colors(), badger_palette)
  expect_error(badger_colors("chartreuse-ish"), "Unknown Badger color")
})

test_that("Badger ggplot scales use standard order and accept mappings", {
  color_scale <- scale_color_badger()
  fill_scale <- scale_fill_badger(
    values = c(Wisconsin = "badred", National = "badblue")
  )

  expect_equal(color_scale$palette(3), unname(badger_palette))
  expect_equal(
    fill_scale$palette(2),
    c(Wisconsin = "#ED0000", National = "#313469")
  )
  expect_s3_class(scale_colour_badger(), "Scale")
})

test_that("Badger scales build with color and fill aesthetics", {
  data <- data.frame(
    x = 1:4,
    y = 1:4,
    group = rep(c("A", "B"), 2)
  )

  color_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x, y, colour = group)
  ) +
    ggplot2::geom_line() +
    scale_color_badger()
  fill_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(group, y, fill = group)
  ) +
    ggplot2::geom_col() +
    scale_fill_badger()

  expect_silent(ggplot2::ggplot_build(color_plot))
  expect_silent(ggplot2::ggplot_build(fill_plot))
})
