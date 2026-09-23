# Run from a source checkout after pkgload::load_all(), or with an installed
# badgerstyle package. Timings include plot building, rendering, and PNG writing.
# Optional environment variable BADGER_BENCH_ITERATIONS controls repetitions.
library(badgerstyle)
library(ggplot2)

iterations <- as.integer(Sys.getenv("BADGER_BENCH_ITERATIONS", "3"))
stopifnot(length(iterations) == 1L, !is.na(iterations), iterations > 0L)
series <- expand.grid(year = 2000:2025, series = LETTERS[1:6])
series$value <- 50 + as.integer(series$series) * 8 +
  (series$year - 2000) * 0.5 + sin(series$year / 2) * 3
plots <- list(
  scatter = ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    badger_style(register_fonts = FALSE),
  lines = ggplot(series, aes(year, value, colour = series)) +
    badger_style(register_fonts = FALSE) + badger_line(lw = 1.4) +
    scale_color_badger() + badger_dynamic_legend(text_family = "sans")
)
# Override the theme's explicit child families too, so this benchmark works on
# machines without the Badger fonts and does not time font-fallback warnings.
plots <- lapply(plots, function(p) p + theme(
  text = element_text(family = "sans"),
  plot.title = element_text(family = "sans"),
  plot.subtitle = element_text(family = "sans"),
  legend.text = element_text(family = "sans"),
  axis.title = element_text(family = "sans"),
  axis.text = element_text(family = "sans")
))
backends <- "png"
if (requireNamespace("ragg", quietly = TRUE)) backends <- c(backends, "ragg")
cases <- expand.grid(plot = names(plots), device = backends, dpi = c(864, 150),
                     stringsAsFactors = FALSE)
path <- tempfile(fileext = ".png")
render <- function(i) {
  badger_finisher(
    plots[[cases$plot[i]]], head = "Rendering benchmark", source = "Source: test data",
    filename = path, dpi = cases$dpi[i], device = cases$device[i],
    register_fonts = FALSE, title_family = "sans", text_family = "sans"
  )
}
# Warm every case before measuring, including font discovery and the logo cache.
for (i in seq_len(nrow(cases))) render(i)
timings <- matrix(NA_real_, nrow(cases), iterations)
set.seed(1)
for (j in seq_len(iterations)) {
  for (i in sample(seq_len(nrow(cases)))) {
    timings[i, j] <- unname(system.time(render(i))[["elapsed"]])
  }
}
unlink(path)
cases$median_seconds <- apply(timings, 1, median)
print(cases, row.names = FALSE)
cat("\nMedian of", iterations, "warm runs per case; default 9.55 x 5 inch size.\n")
cat("R:", as.character(getRversion()), " ggplot2:", as.character(packageVersion("ggplot2")))
if ("ragg" %in% backends) cat(" ragg:", as.character(packageVersion("ragg")))
cat("\nPlatform:", R.version$platform, "\n")
