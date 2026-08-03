#' Print a package greeting
#'
#' Prints `"Hello, world!"` to the console. This function is retained for
#' backward compatibility with early versions of the package.
#'
#' @return The greeting string, invisibly, as returned by [print()].
#'
#' @examples
#' hello()
#'
#' @export
hello <- function() {
  print("Hello, world!")
}
