line_test_data <- function() {
  data.frame(
    year = rep(2020:2022, 2),
    value = c(1, 2, NA, 2, 3, 4),
    series = rep(c("ends early", "complete"), each = 3)
  )
}

test_that("badger_line inherits from the plot on its left", {
  data <- line_test_data()
  width <- 1.4

  specification <- badger_line()
  expect_s3_class(specification, "badger_line")

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(year, value, colour = series)
  ) +
    badger_line(lw = width)

  expect_silent(ggplot2::ggplot_build(plot))
  expect_equal(length(plot$layers), 4L)
  expect_equal(plot$layers[[1L]]$geom_params$lineend, "round")
  expect_equal(plot$layers[[1L]]$aes_params$linewidth, width * 1.45)
  expect_equal(plot$layers[[3L]]$aes_params$linewidth, width)
})

test_that("badger_line preserves the original plot-first API", {
  data <- line_test_data()
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(year, value, colour = series)
  )

  layers <- badger_line(plot, lw = 1.2)

  expect_type(layers, "list")
  expect_equal(length(layers), 4L)
  expect_silent(ggplot2::ggplot_build(plot + layers))

  explicit_layers <- badger_line(
    plot,
    data,
    lw = 1.1,
    group_var = series,
    x_var = year,
    y_var = value
  )
  expect_equal(length(explicit_layers), 4L)

  partially_inherited <- badger_line(
    plot,
    df = data,
    group_var = series,
    x_var = year
  )
  expect_equal(length(partially_inherited), 4L)
})

test_that("badger_line supports explicit data without a plot", {
  data <- line_test_data()

  layers <- badger_line(
    df = data,
    lw = 1,
    group_var = "series",
    x_var = "year",
    y_var = "value"
  )

  expect_equal(length(layers), 4L)
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(year, value, colour = series)
  )
  expect_silent(ggplot2::ggplot_build(plot + layers))
})

test_that("badger_line endpoints use final observed values", {
  data <- line_test_data()
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(year, value, colour = series)
  ) +
    badger_line()

  endpoint_data <- plot$layers[[2L]]$data

  expect_equal(
    sort(endpoint_data$year[endpoint_data$series == "ends early"]),
    c(2020, 2021)
  )
  expect_equal(
    sort(endpoint_data$year[endpoint_data$series == "complete"]),
    c(2020, 2022)
  )
})

test_that("badger_line computes endpoints independently within facets", {
  data <- expand.grid(
    year = 2020:2022,
    series = c("a", "b"),
    panel = c("one", "two")
  )
  data$value <- seq_len(nrow(data))
  data$value[data$panel == "one" & data$series == "a" & data$year == 2022] <- NA

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(year, value, colour = series)
  ) +
    ggplot2::facet_wrap(~panel) +
    badger_line()

  endpoints <- plot$layers[[2L]]$data
  expect_equal(nrow(endpoints), 8L)
  expect_equal(
    max(endpoints$year[endpoints$panel == "one" & endpoints$series == "a"]),
    2021
  )
})

test_that("badger_line remains four layers for many series", {
  data <- data.frame(
    x = rep(1:3, 20),
    y = seq_len(60),
    series = rep(sprintf("series-%02d", 1:20), each = 3)
  )
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = series)) +
    badger_line()

  expect_equal(length(plot$layers), 4L)
})

test_that("badger_line reports missing inherited aesthetics", {
  data <- line_test_data()
  plot <- ggplot2::ggplot(data, ggplot2::aes(year, value))

  expect_error(
    plot + badger_line(),
    "group_var"
  )
})
