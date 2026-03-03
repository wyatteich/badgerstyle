#' Add styled lines with white backdrop to a ggplot
#'
#' Iterates over unique values of a grouping variable and adds a layered
#' line + endpoint combination for each group: a white backdrop line/points
#' for legibility, followed by the colored line/points on top. Returns a list
#' of geoms that can be added to a ggplot object with \code{+}.
#'
#' @param plot A ggplot object. Used to inherit \code{df}, \code{group_var},
#'   and/or \code{x_var} from the plot's \code{data} and \code{mapping} if
#'   those arguments are not supplied explicitly.
#' @param df A data frame. Defaults to \code{plot$data} if not supplied.
#' @param lw Numeric. Base line width. Backdrop is drawn at \code{lw * 1.45}.
#'   Defaults to \code{2.5}.
#' @param group_var Unquoted or quoted variable name for the grouping aesthetic
#'   (typically mapped to \code{color}). Defaults to the \code{colour} mapping
#'   of \code{plot}.
#' @param x_var Unquoted or quoted variable name for the x-axis. Defaults to
#'   the \code{x} mapping of \code{plot}. Passed to \code{find_endpoints()} to
#'   identify min/max x positions for terminal points.
#'
#' @return A list of ggplot layer objects, addable to a ggplot via \code{+}.
#'
#' @seealso \code{\link{find_endpoints}}
#'
#' @examples
#' \dontrun{
#' p <- df |>
#'   ggplot(aes(x = year, y = rank, color = State)) +
#'   badger_style()
#'
#' # Inherit all mappings from plot
#' p + badger_lines(p)
#'
#' # Override line width
#' p + badger_lines(p, lw = 3)
#'
#' # Supply variables explicitly
#' p + badger_lines(p, group_var = State, x_var = year)
#' }
#'
#' @importFrom ggplot2 geom_line geom_point
#' @importFrom dplyr filter
#' @importFrom rlang as_label sym
#'
#' @export
badger_line <- function(plot, df, lw = 2.5, group_var, x_var){

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

  # backdrop width factor
  bwf <- 1.45
  geom_list <- list()

  for(item in unique(df[[group_var]])) {

    geom_list <- c(geom_list, list(
      geom_line(
        data = filter(df, .data[[group_var]] == item),
        linewidth = lw * bwf,
        lineend = "round",
        color = "white",
        show.legend = F,
      ),
      geom_point(
        data = filter(find_endpoints(df, !!sym(x_var)), .data[[group_var]] == item),
        shape = 21,
        size = lw * bwf,
        stroke = lw + 0.5,
        fill = "white",
        color = "white",
        show.legend = F,
      ),
      geom_line(
        data = filter(df, .data[[group_var]] == item),
        linewidth = lw,
        lineend = "round",
      ),
      geom_point(
        data = filter(find_endpoints(df, !!sym(x_var)), .data[[group_var]] == item),
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
