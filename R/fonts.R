.badger_font_cache <- new.env(parent = emptyenv())

#' Register Badger Institute fonts
#'
#' Loads the extrafont database once per R session for the current platform.
#' On Windows, it also registers the two Franklin Gothic device aliases used
#' by Badger graphics. This direct registration keeps the correct fonts
#' available even when the user's extrafont database is empty or out of date.
#' Repeated calls are inexpensive because successful registration is cached.
#'
#' @param force Logical; reload the font database even if it was already
#'   registered in this session.
#' @param quiet Logical; suppress extrafont progress messages.
#'
#' @return Invisibly returns `TRUE` when font registration changed and `FALSE`
#'   when the cached registration and device aliases were already available.
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
  changed <- FALSE

  if (force || !isTRUE(.badger_font_cache[[cache_key]])) {
    if (.Platform$OS.type == "windows") {
      extrafont::loadfonts(device = "win", quiet = quiet)
    } else {
      extrafont::loadfonts(quiet = quiet)
    }
    .badger_font_cache[[cache_key]] <- TRUE
    changed <- TRUE
  }

  if (.Platform$OS.type == "windows") {
    badger_families <- c(
      "Franklin Gothic Medium Cond",
      "Franklin Gothic Demi Cond"
    )
    missing_families <- setdiff(
      badger_families,
      names(grDevices::windowsFonts())
    )
    if (length(missing_families) > 0L) {
      aliases <- stats::setNames(
        lapply(missing_families, grDevices::windowsFont),
        missing_families
      )
      do.call(grDevices::windowsFonts, aliases)
      changed <- TRUE
    }
  }

  invisible(changed)
}
