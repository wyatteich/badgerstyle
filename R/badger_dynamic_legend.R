#' Add a collision-free legend beside a line chart
#'
#' Labels each series beside its final observed point. Labels that would
#' overlap are moved vertically by the smallest amount needed to maintain a
#' minimum gap. Optional arrows connect the moved labels back to their actual
#' endpoints.
#'
#' The horizontal position arguments are offsets beyond the last observed
#' x-value. By default they are fractions of the observed x-range, which makes
#' the same settings useful for numeric years, dates, and date-times. Set
#' `offset_unit = "data"` to express them directly in x-axis units instead.
#' When the plot has fixed x scale limits, its upper limit must include
#' `label_offset`; otherwise the labels would be removed by the scale. The
#' function warns when it detects that situation. Without fixed limits,
#' `right_space` expands the scale automatically.
#'
#' @param plot An optional ggplot object. When supplied, `data`, `x`, `y`, and
#'   `group` can be inherited from its data and mappings. Usually this can be
#'   omitted: when the result is added with `+`, it inherits from the plot on
#'   its left.
#' @param data A data frame. Defaults to `plot$data`.
#' @param x,y,group Unquoted or quoted column names. If omitted, `x` and `y`
#'   are inherited from the matching plot aesthetics, while `group` is
#'   inherited from `colour`, `color`, or `group`.
#' @param label Optional unquoted or quoted column containing the text to draw.
#'   Defaults to `group`.
#' @param labels Optional custom labels. Supply either a function applied to
#'   the endpoint labels or a named character vector whose names match values
#'   of `group`. Unmatched groups retain their original labels.
#' @param by Optional character vector of facet columns. Endpoint selection and
#'   label spacing are performed independently within each combination of
#'   these columns, and the columns are retained so ggplot places each legend
#'   in the correct facet.
#' @param min_gap Minimum vertical distance between labels, in y-axis units.
#'   The default is `min_gap_fraction` of the label range.
#' @param min_gap_fraction Fraction of the label range used when `min_gap` is
#'   `NULL`. Defaults to `0.055`.
#' @param y_limits Two finite increasing values used to position the labels.
#'   Defaults to explicit y scale limits in `plot`, when available, and then
#'   to the range of `y` in `data`.
#' @param floor,ceiling Optional lower and upper label bounds. Defaults to
#'   `bound_padding` inside `y_limits`.
#' @param bound_padding Fraction of `y_limits` reserved above and below the
#'   labels when `floor` or `ceiling` is not supplied. Defaults to `0.035`.
#' @param arrows Logical; draw connecting arrows. Defaults to `TRUE`.
#' @param mask Logical; cover gridlines in the label area. Defaults to `TRUE`.
#' @param hide_legend Logical; hide ggplot's standard legend. Defaults to
#'   `TRUE`.
#' @param offset_unit Interpret horizontal offsets as a `"fraction"` of the
#'   observed x-range or in raw `"data"` units.
#' @param label_offset,arrow_start_offset,arrow_end_offset,mask_offset,right_space
#'   Horizontal offsets beyond the final observed x-value. `right_space`
#'   controls how much room the plot reserves for the labels.
#' @param text_size,text_family,text_color,hjust,vjust,lineheight Text styling
#'   passed to [ggplot2::geom_text()].
#' @param arrow_linewidth,arrow_length,arrow_type Arrow styling. `arrow_length`
#'   is measured in centimeters.
#' @param mask_fill Fill color used to cover gridlines in the label area.
#' @importFrom rlang .data
#'
#' @return When plot information must be inherited, a deferred component that
#'   ggplot resolves when it is added with `+`. Otherwise, a named list of
#'   ggplot layers and, when requested, a theme. The computed endpoint data on
#'   an eagerly built layer list are available as `attr(result, "label_data")`.
#'
#' @examples
#' library(ggplot2)
#'
#' series <- data.frame(
#'   year = rep(2020:2024, 3),
#'   value = c(8:12, 9:13, c(8, 10, 11, 12.5, 13.2)),
#'   place = rep(c("WI", "MN", "MI"), each = 5)
#' )
#'
#' ggplot(series, aes(year, value, colour = place)) +
#'   geom_line() +
#'   badger_dynamic_legend(
#'     min_gap = 1,
#'     labels = c(WI = "Wisconsin", MN = "Minnesota", MI = "Michigan"),
#'     arrows = TRUE,
#'     label_offset = 0.12,
#'     right_space = 0.35,
#'     text_family = "sans"
#'   )
#'
#' @export
badger_dynamic_legend <- function(
    plot = NULL,
    data,
    x,
    y,
    group,
    label,
    labels = NULL,
    by = NULL,
    min_gap = NULL,
    min_gap_fraction = 0.055,
    y_limits = NULL,
    floor = NULL,
    ceiling = NULL,
    bound_padding = 0.035,
    arrows = TRUE,
    mask = TRUE,
    hide_legend = TRUE,
    offset_unit = c("fraction", "data"),
    label_offset = 0.105,
    arrow_start_offset = 0.092,
    arrow_end_offset = 0.04,
    mask_offset = 0.018,
    right_space = 0.28,
    text_size = 3.5,
    text_family = "Franklin Gothic Medium Cond",
    text_color = "#222222",
    hjust = 0,
    vjust = 0.5,
    lineheight = 0.9,
    arrow_linewidth = 0.55,
    arrow_length = 0.11,
    arrow_type = "closed",
    mask_fill = "white") {

  deferred_call <- match.call(expand.dots = FALSE)
  needs_plot <- is.null(plot) && (
    missing(data) || missing(x) || missing(y) || missing(group)
  )
  if (needs_plot) {
    return(structure(
      list(call = deferred_call, environment = parent.frame()),
      class = "badger_dynamic_legend"
    ))
  }

  data_missing <- missing(data)
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  group_quo <- rlang::enquo(group)
  label_quo <- rlang::enquo(label)

  if (!is.null(plot) && !inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object or NULL.", call. = FALSE)
  }

  if (data_missing) {
    if (is.null(plot) || is.null(plot$data)) {
      stop("Supply `data` or a ggplot containing data.", call. = FALSE)
    }
    data <- plot$data
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  x_name <- .badger_legend_column(x_quo, plot, "x", "x")
  y_name <- .badger_legend_column(y_quo, plot, "y", "y")
  group_name <- .badger_legend_column(
    group_quo,
    plot,
    c("colour", "color", "group"),
    "group"
  )
  label_name <- if (rlang::quo_is_missing(label_quo)) {
    group_name
  } else {
    .badger_legend_column(label_quo, plot = NULL, aesthetic = NULL, arg = "label")
  }

  required <- unique(c(x_name, y_name, group_name, label_name, by))
  absent <- setdiff(required, names(data))
  if (length(absent) > 0L) {
    stop(
      "Dynamic legend data is missing: ",
      paste(absent, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.null(by) && (!is.character(by) || anyNA(by))) {
    stop("`by` must be NULL or a character vector of column names.", call. = FALSE)
  }

  offset_unit <- match.arg(offset_unit)
  .badger_scalar_logical(arrows, "arrows")
  .badger_scalar_logical(mask, "mask")
  .badger_scalar_logical(hide_legend, "hide_legend")
  .badger_fraction(min_gap_fraction, "min_gap_fraction", zero_ok = FALSE)
  .badger_fraction(bound_padding, "bound_padding", upper = 0.5)

  offset_values <- list(
    label_offset = label_offset,
    arrow_start_offset = arrow_start_offset,
    arrow_end_offset = arrow_end_offset,
    mask_offset = mask_offset,
    right_space = right_space
  )
  valid_offsets <- vapply(
    offset_values,
    function(value) length(value) == 1L && is.numeric(value) && is.finite(value),
    logical(1)
  )
  if (!all(valid_offsets)) {
    stop("All horizontal offsets must be finite numeric scalars.", call. = FALSE)
  }
  if (right_space < label_offset) {
    stop("`right_space` must be at least as large as `label_offset`.", call. = FALSE)
  }

  y_values <- suppressWarnings(as.numeric(data[[y_name]]))
  x_values <- data[[x_name]]
  x_numeric <- suppressWarnings(as.numeric(x_values))
  valid <- !is.na(x_numeric) & is.finite(y_values) & !is.na(data[[group_name]])
  if (!any(valid)) {
    stop("No rows have valid x, y, and group values.", call. = FALSE)
  }

  data <- data[valid, , drop = FALSE]
  data$.badger_x_numeric <- x_numeric[valid]
  data$.badger_y_numeric <- y_values[valid]

  y_limits <- .badger_legend_y_limits(y_limits, plot, data$.badger_y_numeric)
  y_span <- diff(y_limits)
  if (is.null(floor)) floor <- y_limits[[1L]] + y_span * bound_padding
  if (is.null(ceiling)) ceiling <- y_limits[[2L]] - y_span * bound_padding
  if (
    length(floor) != 1L || length(ceiling) != 1L ||
      !is.finite(floor) || !is.finite(ceiling) || floor >= ceiling
  ) {
    stop("`floor` and `ceiling` must be finite scalars with floor < ceiling.", call. = FALSE)
  }

  gap_was_supplied <- !is.null(min_gap)
  if (is.null(min_gap)) min_gap <- (ceiling - floor) * min_gap_fraction
  if (length(min_gap) != 1L || !is.finite(min_gap) || min_gap < 0) {
    stop("`min_gap` must be a non-negative finite scalar.", call. = FALSE)
  }

  panel_rows <- .badger_panel_rows(data, by)
  panel_results <- lapply(panel_rows, function(rows) {
    panel <- data[rows, , drop = FALSE]
    group_keys <- unique(as.character(panel[[group_name]]))
    endpoint_indices <- vapply(group_keys, function(key) {
      candidates <- which(as.character(panel[[group_name]]) == key)
      candidates[[which.max(panel$.badger_x_numeric[candidates])]]
    }, integer(1))
    endpoints <- panel[endpoint_indices, , drop = FALSE]

    available_gap <- if (nrow(endpoints) <= 1L) {
      Inf
    } else {
      (ceiling - floor) / (nrow(endpoints) - 1L)
    }
    panel_gap <- min_gap
    if (panel_gap > available_gap) {
      if (gap_was_supplied) {
        stop(
          sprintf(
            paste0(
              "`min_gap` is too large: %.4g is requested, but at most ",
              "%.4g fits between `floor` and `ceiling`."
            ),
            panel_gap,
            available_gap
          ),
          call. = FALSE
        )
      }
      panel_gap <- available_gap
    }

    endpoints$.badger_label_y <- .badger_evenly_space(
      endpoints$.badger_y_numeric,
      min_gap = panel_gap,
      floor = floor,
      ceiling = ceiling
    )
    endpoints$.badger_label <- as.character(endpoints[[label_name]])

    panel_min_x <- min(panel$.badger_x_numeric)
    panel_max_x <- max(panel$.badger_x_numeric)
    panel_span <- panel_max_x - panel_min_x
    if (!is.finite(panel_span) || panel_span <= 0) panel_span <- 1
    offset <- function(value) {
      if (offset_unit == "fraction") value * panel_span else value
    }

    endpoints$.badger_label_x_numeric <- panel_max_x + offset(label_offset)
    endpoints$.badger_arrow_start_numeric <- panel_max_x + offset(arrow_start_offset)
    endpoints$.badger_arrow_end_numeric <-
      endpoints$.badger_x_numeric + offset(arrow_end_offset)

    panel_key <- panel[1L, by, drop = FALSE]
    spacer <- panel_key
    spacer$.badger_right_x_numeric <- panel_max_x + offset(right_space)
    spacer$.badger_mid_y <- mean(y_limits)
    mask_data <- panel_key
    mask_data$.badger_mask_x_numeric <- panel_max_x + offset(mask_offset)

    list(endpoints = endpoints, spacer = spacer, mask = mask_data)
  })

  endpoints <- do.call(rbind, lapply(panel_results, `[[`, "endpoints"))
  spacer <- do.call(rbind, lapply(panel_results, `[[`, "spacer"))
  mask_data <- do.call(rbind, lapply(panel_results, `[[`, "mask"))
  rownames(endpoints) <- NULL
  rownames(spacer) <- NULL
  rownames(mask_data) <- NULL

  endpoints$.badger_label <- .badger_custom_labels(
    endpoints$.badger_label,
    endpoints[[group_name]],
    labels
  )

  x_prototype <- x_values
  endpoints$.badger_label_x <- .badger_restore_x(
    endpoints$.badger_label_x_numeric,
    x_prototype
  )
  endpoints$.badger_arrow_start <- .badger_restore_x(
    endpoints$.badger_arrow_start_numeric,
    x_prototype
  )
  endpoints$.badger_arrow_end <- .badger_restore_x(
    endpoints$.badger_arrow_end_numeric,
    x_prototype
  )
  endpoints$.badger_endpoint_x <- .badger_restore_x(
    endpoints$.badger_x_numeric,
    x_prototype
  )
  spacer$.badger_right_x <- .badger_restore_x(
    spacer$.badger_right_x_numeric,
    x_prototype
  )
  mask_data$.badger_mask_x <- .badger_restore_x(
    mask_data$.badger_mask_x_numeric,
    x_prototype
  )
  mask_data$.badger_infinite_x <- .badger_restore_x(Inf, x_prototype)

  .badger_check_legend_x_limits(
    plot,
    max(endpoints$.badger_label_x_numeric, na.rm = TRUE)
  )

  layers <- list(
    space = ggplot2::geom_blank(
      data = spacer,
      mapping = ggplot2::aes(x = .data$.badger_right_x, y = .data$.badger_mid_y),
      inherit.aes = FALSE
    )
  )

  if (mask) {
    layers$mask <- ggplot2::geom_rect(
      data = mask_data,
      mapping = ggplot2::aes(
        xmin = .data$.badger_mask_x,
        xmax = .data$.badger_infinite_x,
        ymin = -Inf,
        ymax = Inf
      ),
      inherit.aes = FALSE,
      fill = mask_fill,
      colour = NA
    )
  }

  if (arrows) {
    arrow_mapping <- ggplot2::aes(
      x = .data$.badger_arrow_start,
      y = .data$.badger_label_y,
      xend = .data$.badger_arrow_end,
      yend = .data$.badger_y_numeric,
      colour = !!rlang::sym(group_name)
    )
    layers$arrows <- ggplot2::geom_segment(
      data = endpoints,
      mapping = arrow_mapping,
      inherit.aes = FALSE,
      linewidth = arrow_linewidth,
      arrow = grid::arrow(
        length = grid::unit(arrow_length, "cm"),
        type = arrow_type
      ),
      show.legend = FALSE
    )
  }

  layers$labels <- ggplot2::geom_text(
    data = endpoints,
    mapping = ggplot2::aes(
      x = .data$.badger_label_x,
      y = .data$.badger_label_y,
      label = .data$.badger_label
    ),
    inherit.aes = FALSE,
    family = text_family,
    colour = text_color,
    size = text_size,
    hjust = hjust,
    vjust = vjust,
    lineheight = lineheight
  )

  if (hide_legend) {
    layers$theme <- ggplot2::theme(legend.position = "none")
  }

  attr(layers, "label_data") <- endpoints
  layers
}

#' Add a deferred dynamic legend to a ggplot
#'
#' @param object A deferred dynamic-legend specification.
#' @param plot The ggplot assembled to the left of the `+` operator.
#' @param object_name Name of the object being added, supplied by ggplot2.
#'
#' @return The plot with the dynamic-legend layers added.
#' @keywords internal
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.badger_dynamic_legend <- function(object, plot, object_name) {
  evaluation_environment <- new.env(parent = object$environment)
  evaluation_environment$.badger_dynamic_legend_function <- badger_dynamic_legend
  evaluation_environment$.badger_inherited_plot <- plot

  replay <- object$call
  replay[[1L]] <- quote(.badger_dynamic_legend_function)
  replay$plot <- quote(.badger_inherited_plot)
  layers <- eval(replay, envir = evaluation_environment)

  plot + layers
}

.badger_legend_column <- function(quo, plot, aesthetic, arg) {
  if (!rlang::quo_is_missing(quo)) {
    expression <- rlang::get_expr(quo)
    if (is.character(expression) && length(expression) == 1L) return(expression)
    return(rlang::as_name(expression))
  }

  if (!is.null(plot)) {
    for (name in aesthetic) {
      mapping <- plot$mapping[[name]]
      if (!is.null(mapping)) return(rlang::as_name(mapping))
    }
  }
  stop("Supply `", arg, "` or map it in `plot`.", call. = FALSE)
}

.badger_legend_y_limits <- function(y_limits, plot, y) {
  if (is.null(y_limits) && !is.null(plot)) {
    scale <- plot$scales$get_scales("y")
    if (!is.null(scale) && !is.function(scale$limits)) y_limits <- scale$limits
  }
  if (is.null(y_limits) || length(y_limits) != 2L || any(!is.finite(y_limits))) {
    y_limits <- range(y, finite = TRUE)
  }
  y_limits <- as.numeric(y_limits)
  if (length(y_limits) != 2L || any(!is.finite(y_limits))) {
    stop("`y_limits` must contain two finite values.", call. = FALSE)
  }
  if (y_limits[[1L]] == y_limits[[2L]]) {
    padding <- if (y_limits[[1L]] == 0) 0.5 else abs(y_limits[[1L]]) * 0.05
    y_limits <- y_limits + c(-padding, padding)
  }
  if (y_limits[[1L]] > y_limits[[2L]]) y_limits <- rev(y_limits)
  y_limits
}

.badger_check_legend_x_limits <- function(plot, label_x) {
  if (is.null(plot)) return(invisible(NULL))
  scale <- plot$scales$get_scales("x")
  if (is.null(scale) || is.function(scale$limits) || length(scale$limits) != 2L) {
    return(invisible(NULL))
  }
  limits <- suppressWarnings(as.numeric(scale$limits))
  if (all(is.finite(limits)) && label_x > max(limits)) {
    warning(
      paste0(
        "The fixed x scale ends before the dynamic labels. Extend its upper ",
        "limit, reduce `label_offset`, or remove the fixed x limits."
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

.badger_panel_rows <- function(data, by) {
  if (length(by) == 0L) return(list(seq_len(nrow(data))))
  keys <- interaction(data[by], drop = TRUE, lex.order = TRUE)
  split(seq_len(nrow(data)), keys, drop = TRUE)
}

.badger_custom_labels <- function(default, group, labels) {
  if (is.null(labels)) return(default)
  if (is.function(labels)) {
    result <- labels(default)
    if (length(result) != length(default)) {
      stop("A `labels` function must return one label per endpoint.", call. = FALSE)
    }
    return(as.character(result))
  }
  if (!is.character(labels) || is.null(names(labels)) || any(!nzchar(names(labels)))) {
    stop("`labels` must be a function or a named character vector.", call. = FALSE)
  }
  replacement <- unname(labels[as.character(group)])
  default[!is.na(replacement)] <- replacement[!is.na(replacement)]
  default
}

.badger_evenly_space <- function(y, min_gap, floor, ceiling) {
  if (length(y) <= 1L) return(pmin(pmax(y, floor), ceiling))
  order_index <- order(y, seq_along(y))
  offsets <- (seq_along(y) - 1L) * min_gap
  target <- y[order_index] - offsets
  fitted <- .badger_pava(target)
  fitted <- pmin(pmax(fitted, floor), ceiling - max(offsets))
  spaced <- fitted + offsets
  result <- numeric(length(y))
  result[order_index] <- spaced
  result
}

.badger_pava <- function(x) {
  if (length(x) <= 1L) return(x)
  values <- numeric(length(x))
  weights <- numeric(length(x))
  lengths <- integer(length(x))
  blocks <- 0L

  for (value in x) {
    blocks <- blocks + 1L
    values[[blocks]] <- value
    weights[[blocks]] <- 1
    lengths[[blocks]] <- 1L
    while (blocks > 1L && values[[blocks - 1L]] > values[[blocks]]) {
      weight <- weights[[blocks - 1L]] + weights[[blocks]]
      values[[blocks - 1L]] <- (
        values[[blocks - 1L]] * weights[[blocks - 1L]] +
          values[[blocks]] * weights[[blocks]]
      ) / weight
      weights[[blocks - 1L]] <- weight
      lengths[[blocks - 1L]] <- lengths[[blocks - 1L]] + lengths[[blocks]]
      blocks <- blocks - 1L
    }
  }
  rep(values[seq_len(blocks)], lengths[seq_len(blocks)])
}

.badger_restore_x <- function(x, prototype) {
  if (inherits(prototype, "Date")) return(as.Date(x, origin = "1970-01-01"))
  if (inherits(prototype, "POSIXct")) {
    timezone <- attr(prototype, "tzone")
    if (is.null(timezone) || !nzchar(timezone[[1L]])) timezone <- "UTC"
    return(as.POSIXct(x, origin = "1970-01-01", tz = timezone[[1L]]))
  }
  x
}

.badger_scalar_logical <- function(x, arg) {
  if (length(x) != 1L || is.na(x) || !is.logical(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}

.badger_fraction <- function(x, arg, zero_ok = TRUE, upper = 1) {
  lower <- if (zero_ok) 0 else 0
  bad <- length(x) != 1L || !is.finite(x) || x < lower || x >= upper
  if (!zero_ok) bad <- bad || x == 0
  if (bad) {
    stop("`", arg, "` must be between 0 and ", upper, ".", call. = FALSE)
  }
  invisible(x)
}
