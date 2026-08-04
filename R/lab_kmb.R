#' Format numbers with K/M/B suffixes
#'
#' A label function for use with ggplot2 scales (or standalone) that
#' abbreviates numbers into human-readable K, M, B notation. Missing and
#' infinite values are retained as missing, `"Inf"`, or `"-Inf"`.
#'
#' @param x A numeric vector.
#' @return A character vector of formatted labels.
#'
#' @examples
#' lab_kmb(c(500, 1500, 2.5e6, 7.8e9))
#' # [1] "500" "1.5k" "2.5M" "7.8B"
#'
#' @export
lab_kmb <- function(x) {
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  out <- rep(NA_character_, length(x))

  is_finite <- is.finite(x)
  is_zero <- is_finite & x == 0
  ax <- abs(x)

  small <- is_finite & ax < 1e3
  thousands <- is_finite & ax >= 1e3 & ax < 1e6
  millions <- is_finite & ax >= 1e6 & ax < 1e9
  billions <- is_finite & ax >= 1e9

  out[small] <- scales::label_number(
    big.mark = ","
  )(x[small])

  out[thousands] <- scales::label_number(
    scale = 1 / 1e3, accuracy = 0.1, big.mark = ",", suffix = "k"
  )(x[thousands])

  out[millions] <- scales::label_number(
    scale = 1 / 1e6, accuracy = 0.1, big.mark = ",", suffix = "M"
  )(x[millions])

  out[billions] <- scales::label_number(
    scale = 1 / 1e9, accuracy = 0.1, big.mark = ",", suffix = "B"
  )(x[billions])

  out[is_zero] <- "0"
  out[is.infinite(x)] <- as.character(x[is.infinite(x)])
  out
}
