# badgerstyle
A custom graphing package for ggplot in R to implement the unique Badger style

Pretty cool how R-Studio can also edit markdown files. 

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
