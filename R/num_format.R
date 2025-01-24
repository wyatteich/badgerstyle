# Numerical Abbreviated format

#' Converts
#' @param from start of range
#' @param to end of range
#' @param by interval between tick marks
#' @param suffix the denomination to convert, and the suffix to attack to the number
#' @param currency add a currency symbol to affix to the front of the value.


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


