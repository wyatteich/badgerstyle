test_that("dynamic legend inherits mappings and spaces endpoint labels", {
  data <- data.frame(
    year = rep(2020:2022, 3),
    value = c(1, 2, 10, 2, 3, 10.2, 4, 5, 10.3),
    series = rep(c("a", "b", "c"), each = 3)
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(year, value, colour = series)
  ) + ggplot2::geom_line()

  result <- badger_dynamic_legend(
    plot,
    min_gap = 1,
    y_limits = c(0, 12),
    floor = 0,
    ceiling = 12,
    labels = c(a = "Alpha", c = "Charlie")
  )
  labels <- attr(result, "label_data")

  expect_named(result, c("space", "mask", "arrows", "labels", "theme"))
  expect_equal(nrow(labels), 3)
  expect_equal(labels$.badger_label, c("Alpha", "b", "Charlie"))
  expect_gte(min(diff(sort(labels$.badger_label_y))), 1 - 1e-10)
  expect_equal(labels$.badger_x_numeric, rep(2022, 3))
  expect_silent(ggplot2::ggplot_build(plot + result))
})

test_that("dynamic legend inherits from the plot on its left", {
  data <- data.frame(
    year = rep(2020:2022, 3),
    value = c(1:3, 2:4, 3:5),
    series = rep(c("a", "b", "c"), each = 3)
  )

  specification <- badger_dynamic_legend()
  expect_s3_class(specification, "badger_dynamic_legend")

  label_map <- c(a = "Alpha")
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(year, value, colour = series)
  ) +
    ggplot2::geom_line() +
    badger_dynamic_legend(
      arrows = FALSE,
      labels = label_map
    )

  expect_silent(ggplot2::ggplot_build(plot))
  expect_equal(length(plot$layers), 4L)
  expect_equal(plot$layers[[4L]]$data$.badger_label, c("Alpha", "b", "c"))
  expect_equal(plot$theme$legend.position, "none")
})

test_that("arrows, mask, and standard legend are independently optional", {
  data <- data.frame(
    x = rep(1:2, 2),
    y = c(1, 2, 2, 3),
    group = rep(c("one", "two"), each = 2)
  )

  result <- badger_dynamic_legend(
    data = data,
    x = x,
    y = y,
    group = group,
    arrows = FALSE,
    mask = FALSE,
    hide_legend = FALSE
  )

  expect_named(result, c("space", "labels"))
})

test_that("label columns, functions, and absolute offsets work", {
  data <- data.frame(
    x = rep(1:3, 2),
    y = c(1:3, 2:4),
    group = rep(c("one", "two"), each = 3),
    display = rep(c("First", "Second"), each = 3)
  )

  result <- badger_dynamic_legend(
    data = data,
    x = "x",
    y = "y",
    group = "group",
    label = "display",
    labels = toupper,
    offset_unit = "data",
    label_offset = 2,
    right_space = 4,
    arrows = FALSE
  )
  labels <- attr(result, "label_data")

  expect_equal(labels$.badger_label, c("FIRST", "SECOND"))
  expect_equal(labels$.badger_label_x_numeric, rep(5, 2))
})

test_that("Date axes and facets retain their types and panel columns", {
  data <- expand.grid(
    date = as.Date("2024-01-01") + 0:2,
    series = c("a", "b"),
    panel = c("top", "bottom")
  )
  data$value <- rep(c(1, 2, 3, 2, 3, 4), 2)

  result <- badger_dynamic_legend(
    data = data,
    x = date,
    y = value,
    group = series,
    by = "panel",
    arrows = FALSE
  )
  labels <- attr(result, "label_data")

  expect_s3_class(labels$.badger_label_x, "Date")
  expect_equal(as.integer(table(labels$panel)), c(2L, 2L))
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(date, value, colour = series)
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~panel)
  expect_silent(ggplot2::ggplot_build(plot + result))
})

test_that("facet variables are inferred from the plot", {
  data <- expand.grid(
    x = 1:3,
    series = c("a", "b"),
    panel = c("one", "two")
  )
  data$y <- seq_len(nrow(data))
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x, y, colour = series)
  ) +
    ggplot2::facet_wrap(~panel)

  layers <- badger_dynamic_legend(plot, arrows = FALSE)
  labels <- attr(layers, "label_data")

  expect_equal(nrow(labels), 4L)
  expect_setequal(unique(labels$panel), c("one", "two"))
})

test_that("coordinate limits and panel fill inform label layout", {
  data <- data.frame(
    x = rep(1:3, 2),
    y = c(10, 11, 12, 12, 13, 14),
    series = rep(c("a", "b"), each = 3)
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x, y, colour = series)
  ) +
    ggplot2::coord_cartesian(ylim = c(0, 100)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "ivory"))

  layers <- badger_dynamic_legend(plot, arrows = FALSE)
  labels <- attr(layers, "label_data")

  expect_true(all(labels$.badger_label_y >= 3.5))
  expect_equal(layers$mask$aes_params$fill, "ivory")
})

test_that("relative offsets operate in transformed x space", {
  data <- data.frame(
    x = rep(c(1, 10, 100), 2),
    y = c(1:3, 2:4),
    series = rep(c("a", "b"), each = 3)
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x, y, colour = series)
  ) +
    ggplot2::scale_x_log10()

  layers <- badger_dynamic_legend(
    plot,
    arrows = FALSE,
    label_offset = 0.1,
    right_space = 0.2
  )
  labels <- attr(layers, "label_data")

  expect_equal(labels$.badger_label_x_numeric, rep(10^2.2, 2), tolerance = 1e-8)
})

test_that("invalid label settings fail informatively", {
  data <- data.frame(
    x = rep(1:2, 3),
    y = c(1, 1, 2, 2, 3, 3),
    group = rep(letters[1:3], each = 2)
  )

  expect_error(
    badger_dynamic_legend(
      data = data,
      x = x,
      y = y,
      group = group,
      min_gap = 6,
      floor = 0,
      ceiling = 10
    ),
    "too large"
  )
  expect_error(
    badger_dynamic_legend(
      data = data,
      x = x,
      y = y,
      group = group,
      labels = c("A", "B", "C")
    ),
    "named character vector"
  )
})

test_that("each series uses its own final non-missing observation", {
  data <- data.frame(
    year = rep(2020:2022, 2),
    value = c(1, 2, NA, 3, 4, 5),
    series = rep(c("ends early", "complete"), each = 3)
  )

  result <- badger_dynamic_legend(
    data = data,
    x = year,
    y = value,
    group = series,
    arrows = FALSE
  )
  labels <- attr(result, "label_data")

  expect_equal(
    setNames(labels$.badger_x_numeric, labels$series),
    c("ends early" = 2021, complete = 2022)
  )
})

test_that("fixed x limits that remove labels trigger a warning", {
  data <- data.frame(
    x = rep(1:3, 2),
    y = c(1:3, 2:4),
    series = rep(c("a", "b"), each = 3)
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x, y, colour = series)
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(limits = c(1, 3))

  expect_warning(
    badger_dynamic_legend(plot),
    "fixed x scale"
  )
})
