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

When no color or group aesthetic is mapped, the full data frame is treated as
one series. Use `colour` (or its `color` alias) to set a fixed line color:

```r
ggplot(df, aes(year, value)) +
  badger_line(colour = badred)
```

## Text annotations and callouts

`badger_annotation()` adds individual notes using the established BTN annotation
treatments. Use `style = "label"` for dark text on a borderless white box,
`style = "callout"` for white text on a dark box with a downward triangle,
or `style = "text"` for plain text. Fonts, padding, and connector styling have
Badger defaults.

```r
ggplot(df, aes(year, value)) +
  badger_style() +
  badger_line(colour = badblue) +
  badger_annotation(
    x = 2020, y = 15, label = "Temporary decline",
    style = "callout"
  )
```

`x` and `y` are the **label position**. To point to a separate observation,
supply `xend` and `yend`; an arrow replaces the triangle automatically:

```r
badger_annotation(
  x = 2018, y = 20, label = "Low point",
  xend = 2020, yend = 12.5
)
```

For several notes, supply a data frame and unquoted column names or expressions:

```r
notes <- data.frame(
  year = c(2020, 2024), value = c(12, 19), note = c("Low", "Latest")
)
badger_annotation(
  x = year, y = value, label = note, data = notes,
  nudge_y = 1, style = "text"
)
```

Use `pointer = "up"` to place a triangle callout below its position,
`pointer = "none"` for a box alone, or `connector = "line"` for an unheaded
connector. `fill = NA` makes a box transparent. Nudges move the label and leave
connector targets unchanged; Date x nudges are in days, POSIXct nudges in seconds.
Facet columns in `notes` are retained. Scalar inputs recycle, but other lengths
must match the number of annotations.

Place labels inside any fixed scale limits and leave room for their text.
This helper styles explicitly positioned annotations; it does not select
observations or resolve collisions automatically. Connector targets are exact:
offset them slightly from endpoint circles to leave clearance. For a secondary
axis, supply positions transformed into the primary plotting scale. Triangle
directions assume Cartesian coordinates. See `?badger_annotation` for all options.

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

### Faster graph updates

Both `badger_finisher()` and `badger_publish()` accept `dpi` and `device`.
Existing calls retain the original `device = "png"` and `dpi = 864` defaults.
To try AGG rendering, install the optional package once:

```r
install.packages("ragg")
```

During iteration, save a lower-resolution draft:

```r
badger_finisher(
  plot,
  head = "Wisconsin employment continues to grow",
  source = "Source: U.S. Bureau of Labor Statistics",
  filename = "employment-draft.png",
  aspect = "web",
  device = "ragg",
  dpi = 150
)
```

For final output, choose the publication's required DPI (or omit `dpi` to keep
864). Physical dimensions, text sizes, and spacing settings are unchanged;
lower DPI reduces pixel detail. At the same size, 150 DPI draws about 33 times
fewer pixels than 864 DPI. This is a pixel-count reduction, not a guaranteed
runtime multiplier.

`device = "auto"` uses ragg when available and otherwise uses the original PNG
device. Explicit `device = "ragg"` reports a missing dependency instead of
silently switching. Backend speed depends on the plot and platform; font
metrics and antialiasing can also differ, so inspect a final export when
switching. ragg discovers installed system fonts directly. Both Franklin Gothic
families must still be installed for the intended typography.

The finisher caches the most recently decoded logo as a native raster, refreshing
it when its path, size, or modification/change timestamp changes. Font
registration is already cached per R session. There is no plot cache: updated
data and layers are rebuilt on each save.

The helpers already write directly to a PNG device and return invisibly.
Changing RStudio's plot-pane backend does not change these exports. Assign your
plot to an object and call the finisher without also printing the plot to avoid
rendering both a preview and a saved file. For expensive analyses, reuse prepared
data and precompute model fits outside the plot when only labels or styling are
changing.

To benchmark full exports locally, run
`source(system.file("benchmarks", "render-performance.R", package = "badgerstyle"))`.
The script compares both backends at 864 and 150 DPI on a scatterplot and a
six-series Badger line chart, including building, drawing, and writing the PNG.

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
