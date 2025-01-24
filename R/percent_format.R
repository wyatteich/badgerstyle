# Percent Format

#' Converts a number into a percent
#' @param from start of range
#' @param to end of range
#' @param by interval between tick marks


percent_format <- function(from = 1, to = 1, by = 1) {
  labs <- paste(as.character(seq(from, to, by)*100), "%", sep = "")

  return(labs)
}

percent_format(from = 0, to = 0.1, by = 0.02)
