#' Add styled lines with white backdrop to a ggplot
#'
#' Iterates over unique values of a grouping variable and adds a layered
#' line + endpoint combination for each group: a white backdrop line/points
#' for legibility, followed by the colored line/points on top. Returns a list
#' of geoms that can be added to a ggplot object with \code{+}.
#'
#' Endpoints are computed per group on rows where \code{y_var} is non-missing,
#' so series that start late, end early, or have internal NAs still receive
#' terminal points at their own first and last observed data point.
#'
#' @param plot A ggplot object. Used to inherit \code{df}, \code{group_var},
#'   \code{x_var}, and/or \code{y_var} from the plot's \code{data} and
#'   \code{mapping} if those arguments are not supplied explicitly.
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
#' @return A list of ggplot layer objects, addable to a ggplot via \code{+}.
#'
#' @seealso \code{\link{find_endpoints}}
#'
#' @importFrom ggplot2 geom_line geom_point
#' @importFrom dplyr filter
#' @importFrom rlang as_label sym
#'
#' @export
badger_line <- function(plot, df, lw = 2.5, group_var, x_var, y_var){

  if(missing(df)) df <- plot$data

  if(missing(group_var)) {
    group_var <- as_label(plot$mapping$colour)
  } else {
    group_var <- as_label(substitute(group_var))
  }

  if(missing(x_var)) {
    x_var <- as_label(plot$mapping$x)
  } else {
    x_var <- as_label(substitute(x_var))
  }

  if(missing(y_var)) {
    y_var <- as_label(plot$mapping$y)
  } else {
    y_var <- as_label(substitute(y_var))
  }

  # backdrop width factor
  bwf <- 1.45
  geom_list <- list()

  for(item in unique(df[[group_var]])) {

    grp_df <- filter(df, .data[[group_var]] == item)
    grp_ends <- find_endpoints(grp_df, !!sym(x_var), !!sym(y_var))

    geom_list <- c(geom_list, list(
      geom_line(
        data = grp_df,
        linewidth = lw * bwf,
        lineend = "round",
        color = "white",
        show.legend = F,
      ),
      geom_point(
        data = grp_ends,
        shape = 21,
        size = lw * bwf,
        stroke = lw + 0.5,
        fill = "white",
        color = "white",
        show.legend = F,
      ),
      geom_line(
        data = grp_df,
        linewidth = lw,
        lineend = "round",
      ),
      geom_point(
        data = grp_ends,
        shape = 21,
        size = lw,
        stroke = lw + 0.5,
        fill = "white",
        show.legend = F,
      )
    ))
  }

  return(geom_list)
}
