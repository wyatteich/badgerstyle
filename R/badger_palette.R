#' Badger Institute color palette
#'
#' The standard six-color palette used across Badger Institute graphics. The
#' individual color objects are provided for compatibility with existing BTN
#' and chapter scripts, while `badger_palette` provides the complete named
#' vector.
#'
#' @format A named character vector of hexadecimal colors. The individual
#'   aliases are one-element character values.
#'
#' @examples
#' badred
#' badger_palette
#' badger_palette[c("badred", "badblue")]
#'
#' @export
badger_palette <- c(
  badred = "#ED0000",
  badblue = "#313469",
  badgreen = "#008610",
  badgold = "#F69800",
  badliblue = "#99CCFF",
  badpurple = "#7439C3"
)

#' @rdname badger_palette
#' @export
badred <- unname(badger_palette[["badred"]])

#' @rdname badger_palette
#' @export
badblue <- unname(badger_palette[["badblue"]])

#' @rdname badger_palette
#' @export
badgreen <- unname(badger_palette[["badgreen"]])

#' @rdname badger_palette
#' @export
badgold <- unname(badger_palette[["badgold"]])

#' @rdname badger_palette
#' @export
badliblue <- unname(badger_palette[["badliblue"]])

#' @rdname badger_palette
#' @export
badlightblue <- badliblue

#' @rdname badger_palette
#' @export
badpurple <- unname(badger_palette[["badpurple"]])

#' Select colors from the Badger palette
#'
#' Accepts the established object names (`"badred"`, `"badblue"`, and so on)
#' or shorter names such as `"red"`, `"blue"`, and `"light blue"`.
#'
#' @param ... Character names of colors to select. With no names, returns the
#'   complete palette.
#'
#' @return A named character vector of hexadecimal colors.
#'
#' @examples
#' badger_colors("red", "blue", "gold")
#' badger_colors("badpurple", "light blue")
#'
#' @export
badger_colors <- function(...) {
  requested <- unlist(list(...), use.names = FALSE)
  if (length(requested) == 0L) return(badger_palette)
  if (!is.character(requested) || anyNA(requested)) {
    stop("Palette names must be non-missing character strings.", call. = FALSE)
  }

  keys <- .badger_palette_keys(requested)
  missing_keys <- is.na(keys)
  if (any(missing_keys)) {
    stop(
      "Unknown Badger color: ",
      paste(unique(requested[missing_keys]), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  result <- unname(badger_palette[keys])
  names(result) <- keys
  result
}

#' Apply the Badger palette to a discrete ggplot scale
#'
#' Uses the standard Badger palette for discrete color or fill aesthetics. By
#' default, colors are assigned in palette order: red, blue, green, gold,
#' light blue, then purple. Use `values` to reorder the palette, select a
#' subset, or provide a named mapping from data values to colors.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_colour_manual()]
#'   or [ggplot2::scale_fill_manual()].
#' @param values Optional color vector. Palette names such as `"badblue"` or
#'   `"gold"` are converted automatically; hexadecimal and other R colors are
#'   passed through unchanged. Named vectors retain their names for explicit
#'   data-value mappings.
#'
#' @return A ggplot2 discrete scale.
#'
#' @examples
#' library(ggplot2)
#' d <- data.frame(x = 1:6, y = 1:6, group = rep(c("A", "B"), 3))
#'
#' ggplot(d, aes(x, y, colour = group)) +
#'   geom_line() +
#'   scale_color_badger()
#'
#' ggplot(d, aes(group, y, fill = group)) +
#'   geom_col() +
#'   scale_fill_badger(values = c(A = "badblue", B = "badgold"))
#'
#' @export
scale_color_badger <- function(..., values = NULL) {
  ggplot2::scale_colour_manual(
    ...,
    values = .badger_palette_values(values)
  )
}

#' @rdname scale_color_badger
#' @export
scale_colour_badger <- function(..., values = NULL) {
  scale_color_badger(..., values = values)
}

#' @rdname scale_color_badger
#' @export
scale_fill_badger <- function(..., values = NULL) {
  ggplot2::scale_fill_manual(
    ...,
    values = .badger_palette_values(values)
  )
}

.badger_palette_values <- function(values) {
  if (is.null(values)) return(unname(badger_palette))
  if (!is.character(values) || length(values) == 0L || anyNA(values)) {
    stop("`values` must be NULL or a non-empty character vector.", call. = FALSE)
  }

  keys <- .badger_palette_keys(values)
  matched <- !is.na(keys)
  values[matched] <- unname(badger_palette[keys[matched]])
  values
}

.badger_palette_keys <- function(values) {
  normalized <- tolower(gsub("[^a-z]", "", values))
  aliases <- c(
    red = "badred",
    badred = "badred",
    blue = "badblue",
    badblue = "badblue",
    green = "badgreen",
    badgreen = "badgreen",
    gold = "badgold",
    badgold = "badgold",
    liblue = "badliblue",
    badliblue = "badliblue",
    lightblue = "badliblue",
    badlightblue = "badliblue",
    purple = "badpurple",
    badpurple = "badpurple"
  )
  unname(aliases[normalized])
}
