#' find_endpoints
#'
#' Returns the rows of `data` at the minimum and maximum of `x`, restricted
#' to rows where `y` is non-missing. This ensures that lines with leading
#' or trailing NA values still receive endpoints at their first and last
#' observed data point, rather than at the global x extremes.
#'
#' @param data The dataframe that you will be drawing your endpoints from
#' @param x The variable along the x-axis.
#' @param y Optional. The variable along the y-axis. If supplied, rows where
#'   `y` is NA are dropped before computing the min/max of `x`.
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
