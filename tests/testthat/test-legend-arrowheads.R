legend_plot_grob <- function(plot) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  ggplot2::ggplotGrob(plot)
}
legend_fixture <- function(values = c(20, 50, 51, 80)) {
  data.frame(x = rep(1:2, length(values)),
    y = as.vector(rbind(values - 5, values)),
    series = rep(letters[seq_along(values)], each = 2))
}
legend_layers <- function(data, ...) badger_dynamic_legend(data = data,
  x = x, y = y, group = series, y_limits = c(0, 100), min_gap = 6, ...)
legend_arrow_grob <- function(data, layers) {
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = series)) + layers
  built <- ggplot2::ggplot_build(plot)
  index <- which(vapply(plot$layers, function(x)
    inherits(x$geom, 'GeomBadgerLegendArrow'), logical(1)))
  plot$layers[[index]]$geom$draw_panel(built$data[[index]],
    built$layout$panel_params[[1]], built$layout$coord,
    arrow = plot$layers[[index]]$geom_params$arrow)
}

test_that('one displaced label gives the entire legend stems', {
  data <- legend_fixture()
  layers <- legend_layers(data)
  labels <- attr(layers, 'label_data')
  expect_identical(labels$.badger_displaced, c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(labels$.badger_label_y, c(20, 47.5, 53.5, 80))
  grob <- legend_arrow_grob(data, layers)
  expect_equal(length(grob$children), 1L)
  expect_s3_class(grob$children[[1]], 'segments')
  expect_equal(length(grob$children[[1]]$x0), 4L)
  expect_true(all(labels$.badger_stem))
})

test_that('separated and exactly spaced labels draw heads without line grobs', {
  for (values in list(c(20, 40, 70), c(20, 26, 32), 50)) {
    data <- legend_fixture(values)
    layers <- legend_layers(data)
    expect_false(any(attr(layers, 'label_data')$.badger_displaced))
    grob <- legend_arrow_grob(data, layers)
    expect_length(grob$children, length(values))
    expect_true(all(vapply(grob$children, inherits, logical(1), 'polygon')))
  }
})

test_that('bounds, tied endpoints, and early endpoints preserve correct targets', {
  bounds <- legend_layers(legend_fixture(c(0, 100)))
  expect_true(all(attr(bounds, 'label_data')$.badger_displaced))
  tied <- legend_layers(legend_fixture(c(50, 50, 50)))
  expect_equal(attr(tied, 'label_data')$.badger_displaced, c(TRUE, FALSE, TRUE))
  early <- legend_fixture(c(20, 70))
  early$x[2] <- 1.5
  labels <- attr(legend_layers(early), 'label_data')
  expect_equal(labels$.badger_endpoint_x, c(1.5, 2))
  expect_false(any(labels$.badger_displaced))
})

test_that('Date facets and logarithmic axes render mixed heads and connectors', {
  data <- legend_fixture()
  data$x <- as.Date('2020-01-01') + data$x * 365
  data <- rbind(transform(data, panel = 'one'), transform(data, panel = 'two'))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = series)) +
    ggplot2::facet_wrap(~panel)
  layers <- badger_dynamic_legend(plot, y_limits = c(0, 100), min_gap = 6,
    text_family = 'sans')
  expect_equal(sum(attr(layers, 'label_data')$.badger_displaced), 4L)
  expect_s3_class(legend_plot_grob(plot + layers), 'gtable')
  data <- legend_fixture()
  data$x <- 10^data$x
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = series)) +
    ggplot2::scale_x_log10()
  expect_s3_class(legend_plot_grob(plot + badger_dynamic_legend(plot,
    y_limits = c(0, 100), min_gap = 6, text_family = 'sans')), 'gtable')
})

test_that('open heads and disabled connectors retain their settings', {
  data <- legend_fixture(c(20, 70))
  grob <- legend_arrow_grob(data, legend_layers(data, arrow_type = 'open'))
  expect_true(all(vapply(grob$children, inherits, logical(1), 'polyline')))
  expect_false('arrows' %in% names(legend_layers(data, arrows = FALSE)))
})

test_that('stem consistency is decided separately for each facet', {
  data <- rbind(transform(legend_fixture(c(20, 50, 51, 80)), panel = 'crowded'),
    transform(legend_fixture(c(20, 40, 60, 80)), panel = 'separated'))
  layers <- legend_layers(data, by = 'panel')
  labels <- attr(layers, 'label_data')
  expect_true(all(labels$.badger_stem[labels$panel == 'crowded']))
  expect_false(any(labels$.badger_stem[labels$panel == 'separated']))
})

test_that('stemless legends reclaim connector space without shrinking text room', {
  tight <- legend_layers(legend_fixture(c(20, 40, 60, 80)))
  wide <- legend_layers(legend_fixture())
  a <- attr(tight, 'label_data'); b <- attr(wide, 'label_data')
  expect_equal(a$.badger_label_x_numeric, rep(2.066, 4))
  expect_equal(a$.badger_arrow_start_numeric, rep(2.053, 4))
  expect_equal(b$.badger_label_x_numeric, rep(2.105, 4))
  expect_equal(b$.badger_arrow_start_numeric, rep(2.092, 4))
  expect_equal(tight$space$data$.badger_right_x_numeric - a$.badger_label_x_numeric,
    wide$space$data$.badger_right_x_numeric - b$.badger_label_x_numeric)
  expect_equal(tight$mask$data$.badger_mask_x, wide$mask$data$.badger_mask_x)
  expect_true(all(a$.badger_arrow_start_numeric > a$.badger_arrow_end_numeric))
})

test_that('compact spacing respects data units, dates, and transformed axes', {
  data <- legend_fixture(c(20, 80))
  absolute <- legend_layers(data, offset_unit = 'data', label_offset = 2,
    arrow_start_offset = 1.8, arrow_end_offset = .8, right_space = 4)
  expect_equal(attr(absolute, 'label_data')$.badger_label_x_numeric, c(3.2, 3.2))
  data$x <- as.Date('2020-01-01') + data$x * 365
  dated <- legend_layers(data)
  expect_equal(as.numeric(attr(dated, 'label_data')$.badger_label_x - max(data$x)), rep(365 * .066, 2))
  data <- legend_fixture(c(20, 80)); data$x <- 10^data$x
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = series)) + ggplot2::scale_x_log10()
  logged <- badger_dynamic_legend(plot, y_limits = c(0, 100), min_gap = 6)
  expect_equal(attr(logged, 'label_data')$.badger_label_x_numeric, rep(10^2.066, 2))
})

test_that('short custom connector gaps are never expanded or reversed', {
  result <- legend_layers(legend_fixture(c(20,80)), label_offset = .06,
    arrow_start_offset = .045, arrow_end_offset = .04)
  expect_equal(attr(result, 'label_data')$.badger_label_x_numeric, rep(2.06, 2))
})
