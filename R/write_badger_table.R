#' Write a Badger-styled Excel table
#'
#' Writes a data frame to an \code{.xlsx} file with the Badger Institute
#' table style applied automatically: navy header with white bold text,
#' Arial 10 throughout, centered numeric columns, left-aligned bold
#' dates and year labels, and an optional italicized source line beneath
#' the data. Column types are auto-detected where possible (dates and
#' generic numerics), with explicit overrides available for the things
#' R's class system can't tell you on its own (a percent looks the same
#' as a count looks the same as a dollar amount, as far as the data is
#' concerned).
#'
#' @param data A data frame or tibble to write. Column names are used
#'   as the header row verbatim, so do any renaming upstream.
#' @param path File path for the output \code{.xlsx}. Parent directory
#'   must already exist; the file will be overwritten if present.
#' @param sheet_name Name for the worksheet. Defaults to \code{"Sheet1"}.
#'   Excel caps this at 31 characters and forbids \code{: \ / ? * [ ]};
#'   openxlsx2 will complain if you violate either.
#' @param col_types Named character vector of column-type overrides,
#'   where names match column names in \code{data} and values are one
#'   of \code{"numeric"}, \code{"pop"}, \code{"dollar"}, \code{"percent"},
#'   \code{"date"}, \code{"year"}, or \code{"text"}. Any column not
#'   listed is auto-detected (\code{Date}/\code{POSIXt} -> \code{"date"},
#'   numeric -> \code{"numeric"}, anything else -> \code{"text"}).
#'   Names that don't match a column in \code{data} trigger a warning
#'   and are otherwise ignored.
#' @param source Optional character string written four rows below the
#'   table in italics. Typically a citation like
#'   \code{"Source: BLS, via FRED (WIUR)"}. Pass \code{NULL} (the
#'   default) to omit.
#'
#' @details
#' Type-to-format mapping:
#' \describe{
#'   \item{\code{numeric}}{\code{0.0} — one decimal, no thousands separator}
#'   \item{\code{pop}}{\code{#,##0} — integer with thousands separator}
#'   \item{\code{dollar}}{\code{$#,##0} — dollar-prefixed integer with separator}
#'   \item{\code{percent}}{\code{0.00\%} — two-decimal percent}
#'   \item{\code{date}}{\code{yyyy-mm-dd}, left-aligned, bold}
#'   \item{\code{year}}{plain integer, left-aligned, bold}
#'   \item{\code{text}}{left-aligned, no number format}
#' }
#'
#' Excel format strings use \code{#} and \code{0} as digit placeholders
#' and \code{,} between them as a thousands separator. A trailing comma
#' (e.g. \code{"0,"}) means "scale by 1,000 per comma" — a real format
#' code, not a typo, but easy to write by accident. The formats above
#' have been chosen to do what you'd expect.
#'
#' Column widths are a flat 20 characters for everything except date
#' columns, which get 12. If you need finer control, post-process the
#' returned workbook object before saving, or modify this function.
#'
#' @return Invisibly returns the \code{wbWorkbook} object after saving,
#'   so you can chain further \code{wb_*} modifications if needed (and
#'   re-save manually).
#'
#' @examples
#' \dontrun{
#' library(openxlsx2)
#'
#' tab <- data.frame(
#'   Date = seq(as.Date("2024-01-01"), by = "month", length.out = 6),
#'   `WI unemployment rate` = c(0.031, 0.030, 0.029, 0.030, 0.031, 0.032),
#'   check.names = FALSE
#' )
#'
#' write_badger_table(
#'   data       = tab,
#'   path       = "wi_unemployment.xlsx",
#'   sheet_name = "WI Unemployment",
#'   col_types  = c("WI unemployment rate" = "percent"),
#'   source     = "Source: BLS, via FRED (WIUR)"
#' )
#' }
#'
#' @importFrom openxlsx2 wb_workbook wb_dims wb_color
#' @export
write_badger_table <- function(data, path, sheet_name = "Sheet1",
                               col_types = NULL, source = NULL) {

  # ---- input checks --------------------------------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("`data` has zero rows; nothing to write.", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L) {
    stop("`path` must be a single character string.", call. = FALSE)
  }

  valid_types <- c("numeric", "pop", "dollar", "percent",
                   "date", "year", "text")

  if (!is.null(col_types)) {
    if (is.null(names(col_types))) {
      stop("`col_types` must be a *named* character vector.", call. = FALSE)
    }
    bad_type <- setdiff(col_types, valid_types)
    if (length(bad_type)) {
      stop("Unknown column type(s): ",
           paste(shQuote(bad_type), collapse = ", "),
           ". Valid types: ",
           paste(shQuote(valid_types), collapse = ", "), ".",
           call. = FALSE)
    }
    bad_name <- setdiff(names(col_types), names(data))
    if (length(bad_name)) {
      warning("`col_types` names not found in `data` (ignored): ",
              paste(shQuote(bad_name), collapse = ", "),
              call. = FALSE)
    }
  }

  # ---- type resolution: auto-detect, then overrides win --------------
  detected <- vapply(data, function(x) {
    if (inherits(x, "Date") || inherits(x, "POSIXt")) "date"
    else if (is.numeric(x))                           "numeric"
    else                                              "text"
  }, character(1))

  if (!is.null(col_types)) {
    keep <- intersect(names(col_types), names(data))
    detected[keep] <- col_types[keep]
  }

  # ---- format dictionary --------------------------------------------
  num_fmts <- c(
    numeric = "0.0",
    pop     = "#,##0",
    dollar  = "$#,##0",
    percent = "0.00%",
    date    = "yyyy-mm-dd",
    year    = "0",
    text    = "@"
  )

  widths <- ifelse(detected == "date", 12, 20)
  n         <- nrow(data)
  data_rows <- 2:(n + 1L)
  n_cols    <- length(data)

  # ---- build workbook -----------------------------------------------
  wb <- openxlsx2::wb_workbook()$
    add_worksheet(sheet_name)$
    add_data(x = data)

  # header row
  wb$add_font(dims  = openxlsx2::wb_dims(rows = 1, cols = seq_len(n_cols)),
              name  = "Arial", size = 10,
              color = openxlsx2::wb_color("white"),
              bold  = TRUE)$
    add_fill(dims  = openxlsx2::wb_dims(rows = 1, cols = seq_len(n_cols)),
             color = openxlsx2::wb_color("#2F5496"))$
    add_cell_style(dims       = openxlsx2::wb_dims(rows = 1, cols = seq_len(n_cols)),
                   horizontal = "center")$
    add_border(dims          = openxlsx2::wb_dims(rows = 1, cols = seq_len(n_cols)),
               bottom_color  = openxlsx2::wb_color("white"),
               bottom_border = "thin")

  # body — per-column font, alignment, number format
  for (i in seq_len(n_cols)) {
    type   <- detected[i]
    dims   <- openxlsx2::wb_dims(rows = data_rows, cols = i)
    halign <- if (type %in% c("date", "year", "text")) "left" else "center"
    bold   <- type %in% c("date", "year")

    wb$add_font(dims = dims, name = "Arial", size = 10, bold = bold)$
      add_cell_style(dims = dims, horizontal = halign)$
      add_numfmt(dims = dims, numfmt = num_fmts[[type]])
  }

  wb$set_col_widths(cols = seq_len(n_cols), widths = widths)

  # optional source line
  if (!is.null(source)) {
    if (!is.character(source) || length(source) != 1L) {
      stop("`source` must be a single character string or NULL.",
           call. = FALSE)
    }
    src_row <- n + 4L
    wb$add_data(x = source, start_row = src_row, start_col = 1)$
      add_font(dims   = openxlsx2::wb_dims(rows = src_row, cols = 1),
               name   = "Arial", size = 10, italic = TRUE)$
      add_cell_style(dims       = openxlsx2::wb_dims(rows = src_row, cols = 1),
                     horizontal = "left")
  }

  wb$save(path, overwrite = TRUE)
  invisible(wb)
}
