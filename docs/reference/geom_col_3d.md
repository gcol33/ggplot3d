# Columns and counts on an x/y lattice

Columns and counts on an x/y lattice

## Usage

``` r
geom_col_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  width = NULL,
  depth = NULL,
  base = 0,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_bar_3d(
  mapping = NULL,
  data = NULL,
  stat = StatCount3D,
  position = "identity",
  ...,
  width = NULL,
  depth = NULL,
  base = 0,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_count_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomCol3D,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- width, depth:

  Cell dimensions in transformed x/y units; defaults to 0.9 times
  spacing.

- base:

  Baseline in original z units, overridden by zmin.

- shade:

  Whether to apply lighting.

- geom:

  Registered geometry prototype.

## Value

A ggplot2 layer. Columns accept z or zmax and optional zmin.

## Examples

``` r
d <- expand.grid(x = 1:3, y = 1:3)
d$z <- seq_len(nrow(d)) - 4
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) +
  geom_col_3d() + coord_3d()
ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), factor(gear))) +
  geom_bar_3d() + coord_3d()
ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), factor(gear))) +
  stat_count_3d() + coord_3d()
```
