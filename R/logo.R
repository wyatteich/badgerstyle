.badger_logo_cache <- new.env(parent = emptyenv())

.badger_read_logo <- function(path) {
  # Retain only the latest logo so custom files cannot grow the session cache.
  # Checking metadata on every call also picks up ordinary edits/replacements.
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  info <- file.info(path)
  key <- list(path = path, size = info$size, mtime = info$mtime, ctime = info$ctime)
  if (!identical(key, .badger_logo_cache$key)) {
    # A native raster avoids repeatedly converting a large RGB array in grid.
    img <- png::readPNG(path, native = TRUE)
    .badger_logo_cache$image <- img
    .badger_logo_cache$key <- key
  }
  .badger_logo_cache$image
}
