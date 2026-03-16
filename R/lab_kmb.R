#' Format numbers with K/M/B suffixes
#'
#' A label function for use with ggplot2 scales (or standalone) that
#' abbreviates numbers into human-readable K, M, B notation.
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
  out <- character(length(x))

  is_zero <- x == 0
  ax <- abs(x)

  out[ax < 1e3] <- scales::label_number(
    big.mark = ","
  )(x[ax < 1e3])

  out[ax >= 1e3 & ax < 1e6] <- scales::label_number(
    scale = 1 / 1e3, big.mark = ",", suffix = "k"
  )(x[ax >= 1e3 & ax < 1e6])

  out[ax >= 1e6 & ax < 1e9] <- scales::label_number(
    scale = 1 / 1e6, big.mark = ",", suffix = "M"
  )(x[ax >= 1e6 & ax < 1e9])

  out[ax >= 1e9] <- scales::label_number(
    scale = 1 / 1e9, big.mark = ",", suffix = "B"
  )(x[ax >= 1e9])

  out[is_zero] <- "0"
  out
}
