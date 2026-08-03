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
Install those fonts and register them with `extrafont` before producing final
publication files.

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
same settings work with numeric years and dates. Use `offset_unit = "data"`
for offsets in raw x-axis units, and `arrows = FALSE` when connectors are not
needed.

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
