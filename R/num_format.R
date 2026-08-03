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


num_format <- function(from = 1, to = 1, by = 1, suffix = c("k", "m", "b"), currency = NULL) {

  if(suffix == "k") {
    labs <- paste(as.character(seq(from, to, by)/1000), "k", sep = "")
  } else if (suffix == "m") {
    labs <- paste(as.character(seq(from, to, by)/1000000), "m", sep = "")
  } else if (suffix == "b") {
    labs <- paste(as.character(seq(from, to, by)/1000000000), "b", sep = "")
  }


  labs <- paste(currency, labs, sep = "")

  return(labs)
}


