test_that("annotations are independent of plot data and aesthetics", {
  annotations <- badger_annotation(2, 4, "Wisconsin's peak", text_family = "sans")
  plot <- ggplot2::ggplot(
    data.frame(year = 1:3, value = 1:3, series = letters[1:3]),
    ggplot2::aes(year, value, colour = series)
  ) + ggplot2::geom_point() + annotations
  built <- ggplot2::ggplot_build(plot)$data[[2]]
  expect_equal(nrow(built), 1)
  expect_equal(built$label, "Wisconsin\u2019s peak")
  expect_equal(built$colour, "#222222")
  expect_equal(built$fill, "white")
  border <- if ("linewidth" %in% names(built)) built$linewidth else
    annotations$label$geom_params$label.size
  expect_equal(border, 0)
  expect_false(annotations$label$inherit.aes)
  expect_false(annotations$label$show.legend)
})

test_that("callouts use the BTN triangle and boxed text conventions", {
  down <- badger_annotation(2, 4, "Peak", style = "callout")
  expect_named(down, c("pointer", "label"))
  expect_equal(down$pointer$aes_params$shape, 25)
  expect_equal(down$pointer$aes_params$fill, "#222222")
  expect_equal(down$label$aes_params$colour, "white")
  expect_equal(down$label$aes_params$vjust, 0)
  up <- badger_annotation(2, 4, "Peak", style = "callout", pointer = "up")
  expect_equal(up$pointer$aes_params$shape, 24)
  expect_equal(up$label$aes_params$vjust, 1)
  none <- badger_annotation(2, 4, "Peak", style = "callout", pointer = "none")
  expect_named(none, "label")
  expect_equal(none$label$aes_params$vjust, 0.5)
})

test_that("plain text and transparent labels can be selected", {
  plain <- badger_annotation(1, 1, "Note", style = "text")
  expect_s3_class(plain$label$geom, "GeomText")
  transparent <- badger_annotation(1, 1, "Note", fill = NA, text_color = badred)
  expect_true(is.na(transparent$label$aes_params$fill))
  expect_equal(transparent$label$aes_params$colour, badred)
})

test_that("connectors point to unchanged targets after label nudges", {
  annotations <- badger_annotation(
    1, 5, "Low", xend = 3, yend = 2, nudge_x = 0.5, nudge_y = 1,
    style = "callout", text_family = "sans"
  )
  expect_named(annotations, c("connector", "label"))
  built <- ggplot2::ggplot_build(ggplot2::ggplot() + annotations)$data
  expect_equal(built[[1]]$x, 1.5)
  expect_equal(built[[1]]$y, 6)
  expect_equal(built[[1]]$xend, 3)
  expect_equal(built[[1]]$yend, 2)
  expect_equal(annotations$connector$geom_params$arrow$ends, 2L)
  line <- badger_annotation(1, 5, "Low", xend = 3, yend = 2, connector = "line")
  expect_null(line$connector$geom_params$arrow)
  none <- badger_annotation(1, 5, "Low", xend = 3, yend = 2, connector = "none")
  expect_named(none, "label")
})

test_that("data expressions, scalar recycling, and facets work", {
  notes <- data.frame(year = c(2020, 2024), value = c(1, 3), panel = c("a", "b"))
  original <- notes
  annotations <- badger_annotation(
    year, value + 1, paste("Year", year), data = notes, text_family = "sans"
  )
  resolved <- attr(annotations, "annotation_data")
  expect_equal(resolved$.badger_annotation_y, c(2, 4))
  expect_equal(resolved$.badger_annotation_label, c("Year 2020", "Year 2024"))
  expect_identical(notes, original)
  plot <- ggplot2::ggplot(notes, ggplot2::aes(year, value)) +
    ggplot2::geom_point() + ggplot2::facet_wrap(~panel) + annotations
  built <- ggplot2::ggplot_build(plot)$data[[2]]
  expect_equal(as.integer(table(built$PANEL)), c(1L, 1L))
  direct <- badger_annotation(1:2, 3, c("A", "B"))
  expect_equal(attr(direct, "annotation_data")$.badger_annotation_y, c(3, 3))
  named <- badger_annotation(.data[["year"]], value, "Note", data = notes)
  expect_equal(attr(named, "annotation_data")$.badger_annotation_x, notes$year)
})

test_that("Date and POSIXct types survive nudges and connector targets", {
  day <- as.Date("2024-01-01")
  annotations <- badger_annotation(
    day, 10, "Note", xend = day + 10, yend = 8, nudge_x = 2, text_family = "sans"
  )
  resolved <- attr(annotations, "annotation_data")
  expect_identical(resolved$.badger_annotation_x, day + 2)
  expect_identical(resolved$.badger_annotation_xend, day + 10)
  expect_silent(ggplot2::ggplot_build(ggplot2::ggplot() + annotations))
  time <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  timed <- badger_annotation(time, 10, "Note", nudge_x = 60)
  expect_identical(attr(timed, "annotation_data")$.badger_annotation_x, time + 60)
})

test_that("categories and transformed scales keep explicit coordinates", {
  categorical <- badger_annotation("Wisconsin", 10, "Note", style = "text")
  expect_silent(ggplot2::ggplot_build(ggplot2::ggplot() + categorical))
  log_plot <- ggplot2::ggplot() + ggplot2::scale_y_log10() +
    badger_annotation(1, 100, "Note", xend = 2, yend = 10)
  built <- ggplot2::ggplot_build(log_plot)$data[[1]]
  expect_equal(built$y, 2)
  expect_equal(built$yend, 1)
  flipped <- ggplot2::ggplot() + categorical + ggplot2::coord_flip()
  expect_silent(ggplot2::ggplot_build(flipped))
})

test_that("invalid settings and partial recycling fail clearly", {
  expect_error(badger_annotation(1:3, 1:2, "Note"), "row count")
  expect_error(badger_annotation(NA_real_, 2, "Note"), "missing")
  expect_error(badger_annotation(Inf, 2, "Note"), "finite")
  expect_error(badger_annotation(1, 2, NA_character_), "missing")
  expect_error(badger_annotation(1, 2, "Note", xend = 3), "both")
  expect_error(badger_annotation(1, 2, "Note", connector = "arrow"), "requires")
  expect_error(badger_annotation("WI", 2, "Note", nudge_x = 1), "categorical")
  expect_error(badger_annotation(1, 2, "Note", text_size = -1), "positive")
  expect_error(badger_annotation(1, 2, "Note", nudge_y = Inf), "finite")
  expect_error(badger_annotation(1, 2, "Note", fill = "bad color"), "valid color")
  expect_error(badger_annotation(1, 2, "Note", data = list()), "data frame")
  expect_error(badger_annotation(1, 2, "Note", label_padding = -1), "negative")
  expect_error(badger_annotation(numeric(), numeric(), character()), "row count")
  expect_error(badger_annotation(1, 2, "Note", data = data.frame(
    .badger_annotation_x = 1)), "reserved")
})

test_that("empty annotation tables add no layers", {
  empty <- data.frame(x = numeric(), y = numeric(), label = character())
  layers <- badger_annotation(x, y, label, data = empty)
  expect_length(layers, 0)
  expect_equal(nrow(attr(layers, "annotation_data")), 0)
})
