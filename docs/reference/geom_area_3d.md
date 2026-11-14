# Filled area walls and waterfall curtains

Draws each group as a filled curtain from `base` to z, ordered by x. The
x coordinates shared by stacked groups form their lattice. Groups with
different x coordinates retain their own vertices.

## Usage

``` r
geom_area_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = position_stack_3d(),
  ...,
  base = 0,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_waterfall_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  base = 0,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- base:

  Baseline in original z units, overridden by zmin.

- shade:

  Whether to apply lighting.

## Value

A ggplot2 layer.

## Examples

``` r
d <- expand.grid(x = 0:8, y = 1:3)
d$z <- 1 + sin(d$x / 2 + d$y)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = y, fill = factor(y))) +
  geom_waterfall_3d() + coord_3d()
d <- expand.grid(x = 0:8, y = 0, part = c("a", "b"))
d$z <- 1 + sin(d$x / 2 + as.integer(d$part)) / 2
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = part, fill = part)) +
  geom_area_3d() + coord_3d()
```
