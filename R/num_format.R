#' Format a numeric sequence with abbreviated units
#'
#' Creates labels for an evenly spaced numeric sequence, scaling each value to
#' thousands, millions, or billions and appending the requested suffix.
#'
#' @param from,to Numeric endpoints of the sequence to label.
#' @param by Numeric interval between values.
#' @param suffix One of `"k"`, `"m"`, or `"b"`, selecting thousands,
#'   millions, or billions. Supply a single value.
#' @param currency Optional character prefix such as `"$"`.
#'
#' @return A character vector of formatted labels.
#'
#' @examples
#' num_format(0, 3e6, 1e6, suffix = "m", currency = "$")
#' # [1] "$0m" "$1m" "$2m" "$3m"
#'
#' @seealso [lab_kmb()]
#' @export


num_format <- function(from = 1, to = 1, by = 1, suffix = "k", currency = NULL) {
  suffix <- match.arg(suffix, c("k", "m", "b"))
  numeric_arguments <- c(from = from, to = to, by = by)
  if (any(lengths(list(from, to, by)) != 1L) || any(!is.finite(numeric_arguments))) {
    stop("`from`, `to`, and `by` must be finite numeric scalars.", call. = FALSE)
  }
  if (by == 0) stop("`by` must not be zero.", call. = FALSE)
  if (!is.null(currency) && (length(currency) != 1L || !is.character(currency))) {
    stop("`currency` must be NULL or a single character string.", call. = FALSE)
  }

  divisors <- c(k = 1e3, m = 1e6, b = 1e9)
  prefix <- if (is.null(currency)) "" else currency
  paste0(prefix, as.character(seq(from, to, by) / divisors[[suffix]]), suffix)
}


