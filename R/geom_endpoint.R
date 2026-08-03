#' Add styled endpoints to a line
#'
#' Selects observations at the minimum and maximum x-values and returns a
#' ggplot point layer using Badger's white-filled endpoint style.
#'
#' @param data A data frame containing the plotted observations.
#' @param x Unquoted column mapped to the x-axis.
#' @param y Unquoted column mapped to the y-axis.
#' @param color A fixed color for the endpoint outlines.
#'
#' @return A ggplot2 point layer addable to a plot with `+`.
#'
#' @examples
#' library(ggplot2)
#' d <- data.frame(year = 2020:2024, value = c(2, 4, 3, 5, 6))
#' ggplot(d, aes(year, value)) +
#'   geom_line() +
#'   geom_endpoint(d, year, value, color = "#2F5496")
#'
#' @seealso [find_endpoints()]
#' @export


geom_endpoint <- function(data, x, y, color) {

  endpoints <- dplyr::bind_rows(
    dplyr::filter(data, {{x}} == max({{x}})),
    dplyr::filter(data, {{x}} == min({{x}}))
  )

  ggplot2::geom_point(data = endpoints,
                      ggplot2::aes(x = {{x}}, y = {{y}}),
                      color = color,
                      fill = "white",
                      shape = 21, size = 3.7, stroke = 4.2)



}
