# year_format

#' Converts a year in the format YYYY to 'YY.
#' @param from the start year
#' @param to the end year
#' @param by the interval between years


year_format <- function(from = 1, to = 1, by = 1) {
  labs <- paste(
    "’",
    substring(
      as.character(
        seq(
          from,
          to,
          by)),
      3,
      4),
    sep = "")
  return(labs)
}
