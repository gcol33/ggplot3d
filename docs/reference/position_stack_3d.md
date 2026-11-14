# Stack or fill intervals along an axis

`position_stack_3d()` places the intervals of groups that share a cell
end to end along `axis`; `position_fill_3d()` also rescales each stack
to span 0 to 1. Positive and negative values stack separately. A cell is
the set of rows with the same position on the two other axes.

## Usage

``` r
position_stack_3d(axis = "z", vjust = 1, reverse = FALSE)

position_fill_3d(axis = "z", vjust = 1, reverse = FALSE)
```

## Arguments

- axis:

  Axis to stack along: `"z"`, `"x"` or `"y"`.

- vjust:

  Fraction of each interval at which the anchor (`z` for columns, the
  coordinate of points and text) is placed.

- reverse:

  Reverse the stacking order of the groups. By default the first group
  ends on top, as in
  [`ggplot2::position_stack()`](https://ggplot2.tidyverse.org/reference/position_stack.html).

## Value

A ggplot2 position adjustment.

## Details

Columns stack along z from a zero baseline. They stack along x or y when
`xmin = 0` and `xmax` (or `ymin`, `ymax`) give each column's length.
Bands and walls stack along the axis on which they span from `x`, `y` or
`z` to the matching `*end`; groups stack where they share vertices.
Points, paths, text and drop lines take their coordinate on `axis` as
the length, so labels can follow stacked columns. Stacking happens on
the transformed scale and needs a linear continuous scale on `axis`.

## See also

[`ggplot2::position_stack()`](https://ggplot2.tidyverse.org/reference/position_stack.html),
[`position_dodge_3d()`](https://gillescolling.com/ggplot3d/reference/position_dodge_3d.md).

## Examples

``` r
d <- expand.grid(x = 1:3, y = 1:2, part = c("a", "b", "c"))
d$z <- c(1:6, 6:1, rep(2, 6))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = part)) +
  geom_col_3d(position = position_stack_3d()) + coord_3d()

ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = part)) +
  geom_col_3d(position = position_fill_3d()) + coord_3d()

# Stacked walls: one stacked area per y slice
w <- expand.grid(x = seq(0, 6, by = 0.25), y = 1:2, part = c("a", "b"))
w$h <- 1 + sin(w$x + as.integer(w$part)) / 2
ggplot2::ggplot(w, ggplot2::aes(x, y, zmin = 0, zmax = h, fill = part,
                                group = interaction(part, y))) +
  geom_band_3d(position = position_stack_3d()) + coord_3d()
```
