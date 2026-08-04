.badger_infer_facet_vars <- function(plot, data_names) {
  if (is.null(plot) || is.null(plot$facet) || is.null(plot$facet$params)) {
    return(character())
  }

  params <- plot$facet$params
  facets <- c(params$facets, params$rows, params$cols)
  if (length(facets) == 0L) return(character())

  variables <- vapply(facets, function(facet) {
    expression <- rlang::get_expr(facet)
    if (is.symbol(expression)) rlang::as_name(expression) else NA_character_
  }, character(1))
  intersect(unique(stats::na.omit(variables)), data_names)
}

.badger_split_rows <- function(data, columns) {
  if (length(columns) == 0L) return(list(seq_len(nrow(data))))

  pieces <- lapply(data[columns], function(value) {
    value <- as.character(value)
    value[is.na(value)] <- "<NA>"
    paste0(nchar(value), ":", value)
  })
  keys <- do.call(paste, c(pieces, sep = "|"))
  split(seq_len(nrow(data)), factor(keys, levels = unique(keys)), drop = TRUE)
}

.badger_plot_limits <- function(plot, aesthetic) {
  if (is.null(plot)) return(NULL)

  coordinate_limits <- plot$coordinates$limits[[aesthetic]]
  if (length(coordinate_limits) == 2L) return(coordinate_limits)

  scale <- plot$scales$get_scales(aesthetic)
  if (is.null(scale) || is.function(scale$limits) || length(scale$limits) != 2L) {
    return(NULL)
  }
  scale$limits
}

.badger_x_transformer <- function(plot, prototype) {
  scale <- if (is.null(plot)) NULL else plot$scales$get_scales("x")
  transformation <- if (!is.null(scale) && is.function(scale$get_transformation)) {
    scale$get_transformation()
  } else {
    scales::transform_identity()
  }

  list(
    transform = function(value) {
      restored <- .badger_restore_x(value, prototype)
      suppressWarnings(as.numeric(transformation$transform(restored)))
    },
    inverse = function(value) {
      suppressWarnings(as.numeric(transformation$inverse(value)))
    }
  )
}

.badger_panel_fill <- function(plot) {
  plot_theme <- ggplot2::theme_get()
  if (!is.null(plot) && length(plot$theme) > 0L) plot_theme <- plot_theme + plot$theme
  background <- ggplot2::calc_element("panel.background", plot_theme)
  fill <- background$fill
  if (is.null(fill) || length(fill) != 1L || is.na(fill)) "white" else fill
}
