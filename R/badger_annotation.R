#' Add Badger-style annotations to a chart
#'
#' Draws borderless labels, dark callouts with triangular pointers, or plain
#' text using the Badger annotation conventions. Optional connectors point
#' from the label to a separate observation. Add the result to a ggplot with
#' `+`, after the chart's data layers.
#'
#' @param x,y Coordinates of the label, before nudging. Supply vectors directly
#'   or expressions evaluated in `data`. Dates and date-times are supported.
#' @param label Annotation text, supplied directly or as a column/expression in
#'   `data`. Straight apostrophes are converted to typographic apostrophes.
#' @param data Optional data frame containing annotation columns. Unquoted
#'   column names and `.data[["column"]]` expressions are supported. All columns
#'   are retained so facet variables can place annotations in specific panels.
#'   Without facet columns, ggplot repeats annotations in every panel.
#' @param style One of `"label"` (dark text on white), `"callout"` (white text
#'   on dark fill), or `"text"` (plain dark text).
#' @param xend,yend Optional target coordinates for connectors, evaluated like
#'   `x` and `y`. Supply both. Targets are unaffected by label nudges.
#' @param connector `"auto"` draws an arrow when targets are supplied and
#'   otherwise draws no connector. Alternatively use `"none"`, `"arrow"`, or
#'   `"line"`. Arrows point toward `xend`, `yend`.
#' @param pointer For callouts without connectors, draw a triangle pointing
#'   `"down"` (default), `"up"`, or `"none"`. The label sits above a downward
#'   pointer or below an upward pointer. Connected callouts use the connector
#'   instead of a triangle. Pointers are intended for Cartesian coordinates.
#' @param nudge_x,nudge_y Numeric offsets of the label, in data units. For Date
#'   x coordinates, `nudge_x` is in days; for POSIXct it is in seconds. These
#'   offsets are applied before scale transformations.
#' @param text_size,text_family,text_color Text size in mm, font family, and
#'   color. The default family is Franklin Gothic Medium Cond. `text_color =
#'   NULL` uses the style's default color.
#' @param fill Label background color. `NULL` uses the style default; `NA`
#'   gives a transparent background. Ignored for `style = "text"`.
#' @param hjust,vjust Text justification. The default horizontal justification
#'   is centered. When `vjust` is NULL, triangle callouts are placed on the
#'   appropriate side of their pointer; other labels are centered vertically.
#' @param lineheight Text line-height multiplier.
#' @param label_padding Padding around boxed text, in lines. Corners are square
#'   and borders are suppressed.
#' @param pointer_size Triangle size in mm.
#' @param connector_color,connector_linewidth Connector color and width in mm.
#' @param arrow_length,arrow_type Arrowhead length in cm and type (`"closed"`
#'   or `"open"`).
#'
#' @details
#' Vectors must have length one or the annotation row count; partial recycling
#' is rejected. With `data`, the row count is `nrow(data)`. Coordinates and
#' labels must be non-missing, and numeric coordinates must be finite.
#'
#' The helper does not inherit plot aesthetics, change scales, select extrema,
#' or automatically avoid overlapping labels. Choose label positions and
#' include them within any fixed scale limits. Connectors terminate at exactly
#' the supplied target; place targets slightly outside point markers when
#' clearance is needed. Connectors are drawn before labels so opaque boxes
#' cover the start of the connector. For plain text or transparent boxes,
#' connector starts can remain visible beneath text.
#'
#' For a secondary axis, supply coordinates in the primary plotting scale,
#' using the same transformation as the corresponding series. See
#' [badger_dynamic_legend()] for automatic series endpoint labels.
#'
#' @return A named list of ggplot layers, with resolved annotation data in
#'   `attr(result, "annotation_data")`. Original data columns are preserved;
#'   names starting with `.badger_annotation_` are reserved for this helper.
#'
#' @examples
#' library(ggplot2)
#' observations <- data.frame(year = 2020:2024, value = c(8, 9, 7, 11, 12))
#' p <- ggplot(observations, aes(year, value)) + geom_line()
#'
#' # Dark callout with a downward triangle, above the selected observation.
#' p + badger_annotation(
#'   x = 2022, y = 7.8, label = "Temporary decline", style = "callout",
#'   text_family = "sans"
#' )
#'
#' # A separate label position and arrow target.
#' p + badger_annotation(
#'   x = 2021, y = 11, label = "Low point", xend = 2022, yend = 7.2,
#'   text_family = "sans"
#' )
#'
#' # Multiple labels; facet columns in notes would also be preserved.
#' notes <- data.frame(year = c(2020, 2024), value = c(8, 12),
#'                     note = c("Start", "Latest"))
#' p + badger_annotation(
#'   x = year, y = value, label = note, data = notes,
#'   nudge_y = 0.5, style = "text", text_family = "sans"
#' )
#' @export
badger_annotation <- function(
    x, y, label, data = NULL,
    style = c("label", "callout", "text"),
    xend = NULL, yend = NULL,
    connector = c("auto", "none", "arrow", "line"),
    pointer = c("down", "up", "none"),
    nudge_x = 0, nudge_y = 0,
    text_size = 3.5, text_family = "Franklin Gothic Medium Cond",
    text_color = NULL, fill = NULL,
    hjust = 0.5, vjust = NULL, lineheight = 0.9,
    label_padding = 0.15, pointer_size = 3.5,
    connector_color = "#222222", connector_linewidth = 0.55,
    arrow_length = 0.11, arrow_type = c("closed", "open")) {
  style <- match.arg(style)
  connector <- match.arg(connector)
  pointer <- match.arg(pointer)
  arrow_type <- match.arg(arrow_type)
  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.null(data) && any(startsWith(names(data), ".badger_annotation_"))) {
    stop("Column names starting with `.badger_annotation_` are reserved.", call. = FALSE)
  }

  values <- list(
    x = rlang::eval_tidy(rlang::enquo(x), data = data),
    y = rlang::eval_tidy(rlang::enquo(y), data = data),
    label = rlang::eval_tidy(rlang::enquo(label), data = data),
    xend = rlang::eval_tidy(rlang::enquo(xend), data = data),
    yend = rlang::eval_tidy(rlang::enquo(yend), data = data)
  )
  has_target <- !is.null(values$xend) && !is.null(values$yend)
  if (xor(is.null(values$xend), is.null(values$yend))) {
    stop("Supply both `xend` and `yend` for a connector.", call. = FALSE)
  }
  if (connector == "auto") connector <- if (has_target) "arrow" else "none"
  if (connector != "none" && !has_target) {
    stop("A connector requires both `xend` and `yend`.", call. = FALSE)
  }
  triangle <- style == "callout" && connector == "none" && pointer != "none"
  if (is.null(vjust)) vjust <- if (triangle) {
    if (pointer == "down") 0 else 1
  } else 0.5
  if (is.null(text_color)) text_color <- if (style == "callout") "white" else "#222222"
  if (is.null(fill)) fill <- if (style == "callout") "#222222" else "white"

  numeric_settings <- list(
    nudge_x = nudge_x, nudge_y = nudge_y, hjust = hjust, vjust = vjust,
    text_size = text_size, lineheight = lineheight,
    label_padding = label_padding, pointer_size = pointer_size,
    connector_linewidth = connector_linewidth, arrow_length = arrow_length
  )
  for (name in names(numeric_settings)) {
    value <- numeric_settings[[name]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      stop("`", name, "` must be a finite numeric scalar.", call. = FALSE)
    }
    if (name %in% c("text_size", "lineheight", "pointer_size",
                    "connector_linewidth", "arrow_length") && value <= 0) {
      stop("`", name, "` must be positive.", call. = FALSE)
    }
  }
  if (label_padding < 0) stop("`label_padding` cannot be negative.", call. = FALSE)
  for (name in c("text_color", "fill", "connector_color")) {
    value <- get(name)
    if (length(value) != 1L || !(is.character(value) || identical(value, NA))) {
      stop("`", name, "` must be one color or NA.", call. = FALSE)
    }
    tryCatch(grDevices::col2rgb(value), error = function(e) {
      stop("`", name, "` must be a valid color or NA.", call. = FALSE)
    })
  }
  if (!is.character(text_family) || length(text_family) != 1L || is.na(text_family)) {
    stop("`text_family` must be one font family string.", call. = FALSE)
  }

  if (!has_target) values[c("xend", "yend")] <- NULL
  n <- if (is.null(data)) max(lengths(values)) else nrow(data)
  if (n == 0L && !is.null(data)) return(structure(list(), annotation_data = data))
  for (name in names(values)) {
    value <- values[[name]]
    if (!is.atomic(value) || !is.null(dim(value)) ||
        !(length(value) %in% c(1L, n)) || length(value) == 0L) {
      stop("`", name, "` must have length 1 or the annotation row count (", n, ").",
           call. = FALSE)
    }
    if (anyNA(value)) stop("`", name, "` cannot contain missing values.", call. = FALSE)
    if (name != "label") {
      valid_type <- is.numeric(value) || inherits(value, c("Date", "POSIXct")) ||
        is.character(value) || is.factor(value)
      if (!valid_type || ((is.numeric(value) || inherits(value, c("Date", "POSIXct"))) &&
                          any(!is.finite(as.numeric(value))))) {
        stop("`", name, "` must contain finite numbers, dates, or categories.", call. = FALSE)
      }
    }
    values[[name]] <- rep(value, length.out = n)
  }
  for (axis in c("x", "y")) {
    offset <- numeric_settings[[paste0("nudge_", axis)]]
    if (offset != 0) {
      if (!(is.numeric(values[[axis]]) || inherits(values[[axis]], c("Date", "POSIXct")))) {
        stop("Cannot nudge a categorical `", axis, "`; use explicit label coordinates.",
             call. = FALSE)
      }
      values[[axis]] <- values[[axis]] + offset
      if (any(!is.finite(as.numeric(values[[axis]])))) {
        stop("Nudging `", axis, "` produced non-finite coordinates.", call. = FALSE)
      }
    }
  }
  values$label <- gsub("'", "\u2019", as.character(values$label), fixed = TRUE)
  resolved <- if (is.null(data)) data.frame(row.names = seq_len(n)) else as.data.frame(data)
  for (name in names(values)) resolved[[paste0(".badger_annotation_", name)]] <- values[[name]]

  layers <- list()
  if (connector != "none") {
    layers$connector <- ggplot2::geom_segment(
      data = resolved,
      mapping = ggplot2::aes(
        x = .data$.badger_annotation_x, y = .data$.badger_annotation_y,
        xend = .data$.badger_annotation_xend, yend = .data$.badger_annotation_yend
      ),
      inherit.aes = FALSE, show.legend = FALSE,
      colour = connector_color, linewidth = connector_linewidth,
      arrow = if (connector == "arrow") grid::arrow(
        length = grid::unit(arrow_length, "cm"), type = arrow_type
      ) else NULL
    )
  }
  if (triangle) {
    layers$pointer <- ggplot2::geom_point(
      data = resolved,
      mapping = ggplot2::aes(x = .data$.badger_annotation_x, y = .data$.badger_annotation_y),
      inherit.aes = FALSE, show.legend = FALSE,
      shape = if (pointer == "down") 25 else 24,
      size = pointer_size, stroke = 0, colour = fill, fill = fill
    )
  }
  text_args <- list(
    data = resolved,
    mapping = ggplot2::aes(
      x = .data$.badger_annotation_x, y = .data$.badger_annotation_y,
      label = .data$.badger_annotation_label
    ),
    inherit.aes = FALSE, show.legend = FALSE,
    family = text_family, colour = text_color, size = text_size,
    hjust = hjust, vjust = vjust, lineheight = lineheight
  )
  layers$label <- if (style == "text") {
    do.call(ggplot2::geom_text, text_args)
  } else {
    # ggplot2 3.x controls label borders through label.size; newer releases
    # expose linewidth as an aesthetic instead.
    border_args <- if ("linewidth" %in% names(ggplot2::GeomLabel$default_aes)) {
      list(linewidth = 0)
    } else list(label.size = 0)
    do.call(ggplot2::geom_label, c(text_args, list(
      fill = fill, label.padding = grid::unit(label_padding, "lines"),
      label.r = grid::unit(0, "lines")
    ), border_args))
  }
  attr(layers, "annotation_data") <- resolved
  layers
}
