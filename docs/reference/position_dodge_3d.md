# Dodge groups side by side within a cell

Groups that share an x/y cell are placed next to one another along
`axis`. Columns become narrower so that the dodged set fills the
original cell; points, text and segments move apart by `width / n` per
group.

## Usage

``` r
position_dodge_3d(
  axis = "x",
  width = NULL,
  preserve = "total",
  reverse = FALSE
)
```

## Arguments

- axis:

  Axis to dodge along: `"x"` or `"y"`.

- width:

  Total width taken by the dodged groups in transformed units. Defaults
  to the width of the columns in each cell, or to 0.9 times the lattice
  spacing for geometries without a width.

- preserve:

  `"total"` shares each cell among the groups present in it; `"single"`
  gives every group the width it has in the fullest cell.

- reverse:

  Reverse the order of the groups.

## Value

A ggplot2 position adjustment.

## See also

[`ggplot2::position_dodge()`](https://ggplot2.tidyverse.org/reference/position_dodge.html),
[`ggplot2::position_dodge2()`](https://ggplot2.tidyverse.org/reference/position_dodge.html).

## Examples

``` r
d <- expand.grid(x = 1:3, y = 1:2, part = c("a", "b"))
d$z <- seq_len(nrow(d))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = part)) +
  geom_col_3d(position = position_dodge_3d()) + coord_3d()

ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = part)) +
  geom_col_3d(position = position_dodge_3d(axis = "y")) + coord_3d()
```
