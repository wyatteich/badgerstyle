#' Add styled lines with white backdrop to a ggplot
#'
#' Iterates over unique values of a grouping variable and adds a layered
#' line + endpoint combination for each group: a white backdrop line/points
#' for legibility, followed by the colored line/points on top.
#'
#' Endpoints are computed per group on rows where \code{y_var} is non-missing,
#' so series that start late, end early, or have internal NAs still receive
#' terminal points at their own first and last observed data point.
#'
#' @param plot An optional ggplot object. Used to inherit \code{df},
#'   \code{group_var}, \code{x_var}, and/or \code{y_var} from the plot's
#'   \code{data} and \code{mapping}. Usually this can be omitted: when the
#'   result is added with \code{+}, it inherits from the plot on its left.
#' @param df A data frame. Defaults to \code{plot$data} if not supplied.
#' @param lw Numeric. Base line width. Backdrop is drawn at \code{lw * 1.45}.
#'   Defaults to \code{2.5}.
#' @param group_var Unquoted or quoted variable name for the grouping aesthetic
#'   (typically mapped to \code{color}). Defaults to the \code{colour} mapping
#'   of \code{plot}.
#' @param x_var Unquoted or quoted variable name for the x-axis. Defaults to
#'   the \code{x} mapping of \code{plot}.
#' @param y_var Unquoted or quoted variable name for the y-axis. Defaults to
#'   the \code{y} mapping of \code{plot}. Used to drop NA rows before locating
#'   each group's first and last observed data point.
#'
#' @return When plot information must be inherited, a deferred component that
#'   ggplot resolves when it is added with \code{+}. Otherwise, a list of
#'   ggplot layer objects addable to a ggplot via \code{+}.
#'
#' @seealso \code{\link{find_endpoints}}
#'
#' @examples
#' library(ggplot2)
#'
#' series <- data.frame(
#'   year = rep(2020:2024, 2),
#'   value = c(1:5, 2:6),
#'   place = rep(c("WI", "MN"), each = 5)
#' )
#'
#' # Inherit data and mappings from the plot on the left.
#' ggplot(series, aes(year, value, colour = place)) +
#'   badger_line(lw = 1.4)
#'
#' # The original API remains supported.
#' p <- ggplot(series, aes(year, value, colour = place))
#' p + badger_line(p, lw = 1.4)
#'
#' @export
badger_line <- function(
    plot = NULL,
    df,
    lw = 2.5,
    group_var,
    x_var,
    y_var) {

  deferred_call <- match.call(expand.dots = FALSE)
  needs_plot <- is.null(plot) && (
    missing(df) || missing(group_var) || missing(x_var) || missing(y_var)
  )
  if (needs_plot) {
    return(structure(
      list(call = deferred_call, environment = parent.frame()),
      class = "badger_line"
    ))
  }

  if (!is.null(plot) && !inherits(plot, "ggplot")) {
    stop("`plot` must be a ggplot object or NULL.", call. = FALSE)
  }

  if (missing(df)) df <- plot$data
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  if (missing(group_var)) {
    group_var <- .badger_line_mapping(
      plot,
      c("colour", "color", "group"),
      "group_var"
    )
  } else {
    group_var <- rlang::as_name(substitute(group_var))
  }

  if (missing(x_var)) {
    x_var <- .badger_line_mapping(plot, "x", "x_var")
  } else {
    x_var <- rlang::as_name(substitute(x_var))
  }

  if (missing(y_var)) {
    y_var <- .badger_line_mapping(plot, "y", "y_var")
  } else {
    y_var <- rlang::as_name(substitute(y_var))
  }

  required <- unique(c(group_var, x_var, y_var))
  absent <- setdiff(required, names(df))
  if (length(absent) > 0L) {
    stop(
      "Badger line data is missing: ",
      paste(absent, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (length(lw) != 1L || !is.numeric(lw) || !is.finite(lw) || lw <= 0) {
    stop("`lw` must be a positive finite numeric scalar.", call. = FALSE)
  }

  # backdrop width factor
  bwf <- 1.45
  geom_list <- list()
  group_values <- as.character(df[[group_var]])

  for (item in unique(group_values[!is.na(group_values)])) {

    grp_df <- df[!is.na(group_values) & group_values == item, , drop = FALSE]
    grp_ends <- find_endpoints(
      grp_df,
      !!rlang::sym(x_var),
      !!rlang::sym(y_var)
    )

    geom_list <- c(geom_list, list(
      ggplot2::geom_line(
        data = grp_df,
        linewidth = lw * bwf,
        lineend = "round",
        color = "white",
        show.legend = FALSE
      ),
      ggplot2::geom_point(
        data = grp_ends,
        shape = 21,
        size = lw * bwf,
        stroke = lw + 0.5,
        fill = "white",
        color = "white",
        show.legend = FALSE
      ),
      ggplot2::geom_line(
        data = grp_df,
        linewidth = lw,
        lineend = "round"
      ),
      ggplot2::geom_point(
        data = grp_ends,
        shape = 21,
        size = lw,
        stroke = lw + 0.5,
        fill = "white",
        show.legend = FALSE
      )
    ))
  }

  geom_list
}

#' Add a deferred Badger line to a ggplot
#'
#' @param object A deferred Badger-line specification.
#' @param plot The ggplot assembled to the left of the `+` operator.
#' @param object_name Name of the object being added, supplied by ggplot2.
#'
#' @return The plot with the Badger line layers added.
#' @keywords internal
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.badger_line <- function(object, plot, object_name) {
  evaluation_environment <- new.env(parent = object$environment)
  evaluation_environment$.badger_line_function <- badger_line
  evaluation_environment$.badger_inherited_plot <- plot

  replay <- object$call
  replay[[1L]] <- quote(.badger_line_function)
  replay$plot <- quote(.badger_inherited_plot)
  layers <- eval(replay, envir = evaluation_environment)

  plot + layers
}

.badger_line_mapping <- function(plot, aesthetics, argument) {
  if (!is.null(plot)) {
    for (aesthetic in aesthetics) {
      mapping <- plot$mapping[[aesthetic]]
      if (!is.null(mapping)) return(rlang::as_label(mapping))
    }
  }
  stop(
    "Supply `",
    argument,
    "` or map it in the plot on the left.",
    call. = FALSE
  )
}
