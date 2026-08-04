.badger_font_cache <- new.env(parent = emptyenv())

#' Register Badger Institute fonts with extrafont
#'
#' Loads the extrafont database once per R session for the current platform.
#' Repeated calls are inexpensive because successful registration is cached.
#'
#' @param force Logical; reload the font database even if it was already
#'   registered in this session.
#' @param quiet Logical; suppress extrafont progress messages.
#'
#' @return Invisibly returns `TRUE` when fonts were loaded and `FALSE` when a
#'   cached registration was reused.
#'
#' @examples
#' \dontrun{
#' badger_register_fonts()
#' }
#'
#' @export
badger_register_fonts <- function(force = FALSE, quiet = TRUE) {
  if (length(force) != 1L || !is.logical(force) || is.na(force)) {
    stop("`force` must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(quiet) != 1L || !is.logical(quiet) || is.na(quiet)) {
    stop("`quiet` must be TRUE or FALSE.", call. = FALSE)
  }

  cache_key <- if (.Platform$OS.type == "windows") "windows" else "default"
  if (!force && isTRUE(.badger_font_cache[[cache_key]])) {
    return(invisible(FALSE))
  }

  if (.Platform$OS.type == "windows") {
    extrafont::loadfonts(device = "win", quiet = quiet)
  } else {
    extrafont::loadfonts(quiet = quiet)
  }
  .badger_font_cache[[cache_key]] <- TRUE
  invisible(TRUE)
}
