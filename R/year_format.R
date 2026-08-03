#' Format a sequence of years with abbreviated labels
#'
#' Creates labels containing a right single quotation mark followed by the
#' final two digits of each year.
#'
#' @param from,to Numeric endpoints of the year sequence.
#' @param by Numeric interval between years.
#'
#' @return A character vector of abbreviated year labels.
#'
#' @examples
#' year_format(2020, 2024, 2)
#' # [1] "’20" "’22" "’24"
#'
#' @export
year_format <- function(from = 1, to = 1, by = 1) {
  paste0("\u2019", substring(as.character(seq(from, to, by)), 3, 4))
}
