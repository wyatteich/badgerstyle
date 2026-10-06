#' Format compact numbers without all-zero decimal places
#'
#' A label-function factory for ggplot2 scales. By default, short-scale suffixes
#' are lowercase (k, m, b, t). An all-zero decimal part is removed: 60.0m becomes
#' 60m, while 1.2m remains 1.2m. Nonzero decimal parts retain the chosen accuracy.
#'
#' @param accuracy Rounding accuracy passed to [scales::label_number()].
#' @param decimal.mark Decimal separator passed to [scales::label_number()].
#' @param scale_cut Named numeric vector of scale thresholds. NULL uses
#'   [scales::cut_short_scale()] with lowercase suffixes. Use \code{stats::setNames(0, "")} to
#'   disable automatic abbreviation, for example when supplying a fixed scale.
#' @param ... Other arguments passed to [scales::label_number()], including
#'   \code{prefix}, \code{suffix}, \code{scale}, and \code{big.mark}.
#' @return A function accepting a numeric vector and returning character labels.
#' @examples
#' label_number_trim()(c(0, 60, 1200, 6e7))
#' # "0" "60" "1.2k" "60m"
#' label_number_trim(prefix = "$")(c(1e6, 1.2e6))
#' label_number_trim(scale_cut = stats::setNames(0, ""), scale = 1e-6, suffix = "m")(6e7)
#' @export
label_number_trim <- function(accuracy = 0.1, decimal.mark = ".",
                              scale_cut = NULL, ...) {
  if (is.null(scale_cut)) {
    scale_cut <- scales::cut_short_scale()
    names(scale_cut) <- tolower(names(scale_cut))
  }
  f <- scales::label_number(
    accuracy = accuracy, decimal.mark = decimal.mark, scale_cut = scale_cut, ...
  )
  pat <- paste0(gsub("(\\W)", "\\\\\\1", decimal.mark), "0+(?=\\D*$)")
  function(x) sub(pat, "", f(x), perl = TRUE)
}
