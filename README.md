# badgerstyle

`badgerstyle` provides reusable R tools for producing Badger Institute
graphics and tables. It includes a ggplot2 theme, layered lines and endpoints,
collision-free right-side labels, publication helpers, axis-label formatters,
and styled Excel output.

## Installation

Install the development version from GitHub:

```r
remotes::install_github("wyatteich/badgerstyle")
library(badgerstyle)
```

The graphics functions use the Badger Institute's Franklin Gothic fonts.
Install those fonts before producing final publication files. `badger_style()`
and `badger_finisher()` register them through `extrafont` automatically and
cache the result for the R session. Use `register_fonts = FALSE` to skip that
step or call `badger_register_fonts(force = TRUE)` to reload the font database.

## Standard colors

The established Badger colors are available directly after loading the
package:

```r
badred      # "#ED0000"
badblue     # "#313469"
badgreen    # "#008610"
badgold     # "#F69800"
badliblue   # "#99CCFF"
badpurple   # "#7439C3"
```

Use `badger_palette` for the complete named vector or `badger_colors()` to
select colors by name. The ggplot scales assign these colors to discrete
series in the standard order:

```r
ggplot(df, aes(year, value, colour = series)) +
  badger_line() +
  scale_color_badger()

ggplot(df, aes(category, value, fill = category)) +
  geom_col() +
  scale_fill_badger(values = c("badblue", "badgold", "badgreen"))
```

## Dynamic right-side legends

`badger_dynamic_legend()` labels each line beside its final observed value and
automatically separates crowded labels. It inherits data and aesthetics from
the plot on its left:

```r
ggplot(df, aes(year, value, colour = series)) +
  geom_line() +
  badger_style() +
  badger_dynamic_legend(
    min_gap = 0.075,
    labels = c(wi = "Wisconsin", us = "United States"),
    arrows = TRUE,
    label_offset = 0.11,
    right_space = 0.30
  )
```

Horizontal offsets are fractions of the observed x-range by default, so the
same settings work with numeric years, dates, and transformed axes such as
log scales. Facet variables are inferred for simple `facet_wrap()` and
`facet_grid()` plots so labels are calculated independently by panel. Use
`offset_unit = "data"` for offsets in raw x-axis units, and `arrows = FALSE`
when connectors are not needed.

## Badger lines

`badger_line()` likewise inherits the data and `x`, `y`, and color/group
mappings from the plot on its left:

```r
ggplot(df, aes(year, value, colour = series)) +
  badger_style() +
  badger_line(lw = 1.4)
```

It draws the colored lines and endpoints itself, including the white backdrop,
so a separate `geom_line()` is not needed.

## Publication output

Use `badger_finisher()` to write a high-resolution PNG with a headline, source
line, Badger icon, and optional border:

```r
badger_finisher(
  plot,
  head = "Wisconsin employment continues to grow",
  source = "Source: U.S. Bureau of Labor Statistics",
  filename = "employment.png",
  aspect = "web"
)
```

## Styled Excel tables

`write_badger_table()` writes a data frame to an `.xlsx` file with Badger
header, font, alignment, and number formatting:

```r
write_badger_table(
  data,
  "table.xlsx",
  col_types = c(rate = "percent", population = "pop"),
  source = "Source: U.S. Census Bureau"
)
```

Run `help(package = "badgerstyle")` for the complete function index.
