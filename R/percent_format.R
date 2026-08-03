#' Format a numeric sequence as percentages
#'
#' Creates labels for an evenly spaced sequence of proportions by multiplying
#' each value by 100 and appending a percent sign.
#'
#' @param from,to Numeric endpoints of the sequence to label.
#' @param by Numeric interval between values.
#'
#' @return A character vector of percentage labels.
#'
#' @examples
#' percent_format(0, 0.1, 0.02)
#' # [1] "0%" "2%" "4%" "6%" "8%" "10%"
#'
#' @export


percent_format <- function(from = 1, to = 1, by = 1) {
  labs <- paste(as.character(seq(from, to, by)*100), "%", sep = "")

  return(labs)
}
