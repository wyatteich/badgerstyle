#' Find the first and last observed rows
#'
#' Returns the rows of `data` at the minimum and maximum of `x`, restricted
#' to rows where `y` is non-missing. This ensures that lines with leading
#' or trailing NA values still receive endpoints at their first and last
#' observed data point, rather than at the global x extremes.
#'
#' @param data A data frame containing the plotted observations.
#' @param x Unquoted column used to order observations along the x-axis.
#' @param y Optional unquoted y-axis column. When supplied, rows where `y` is
#'   missing are removed before the x-axis endpoints are selected.
#'
#' @return A data frame containing rows at the minimum and maximum observed
#'   x-values. Tied x-values can produce more than two rows. If no observations
#'   remain after removing missing y-values, an empty data frame is returned.
#'
#' @examples
#' d <- data.frame(year = 2020:2023, value = c(NA, 2, 3, NA))
#' find_endpoints(d, year, value)
#'
#' @seealso [badger_line()], [geom_endpoint()]
#'
#' @export
find_endpoints <- function(data, x, y) {

  if (!missing(y)) {
    data <- dplyr::filter(data, !is.na({{y}}))
  }

  if (nrow(data) == 0) return(data)

  endpoints <- dplyr::bind_rows(
    dplyr::filter(data, {{x}} == max({{x}}, na.rm = TRUE)),
    dplyr::filter(data, {{x}} == min({{x}}, na.rm = TRUE))
  )

  return(endpoints)
}
