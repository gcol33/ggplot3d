# Boxplots and violins standing on an x/y plane

A distribution of z stands at each x/y cell. Map discrete x/y variables,
or set `group = interaction(x, y)` for numeric cells. Boxplots use
ggplot2's boxplot statistic on z, with a box prism, median face and
perimeter, whiskers and outliers. Violin walls mirror the z density
along x; revolved violins sweep the density around the vertical axis.

## Usage

``` r
geom_boxplot_3d(
  mapping = NULL,
  data = NULL,
  stat = StatBoxplot3D,
  position = "identity",
  ...,
  width = NULL,
  depth = NULL,
  coef = 1.5,
  outliers = TRUE,
  fatten = 2.5,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_violin_3d(
  mapping = NULL,
  data = NULL,
  stat = StatViolin3D,
  position = "identity",
  ...,
  shape = c("wall", "revolve"),
  width = NULL,
  depth = NULL,
  bw = "nrd0",
  adjust = 1,
  trim = TRUE,
  n = 80L,
  sides = 16L,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- width, depth:

  Cell dimensions in transformed position units.

- coef:

  Whisker length in multiples of the interquartile range.

- outliers:

  Whether to draw outlier markers. All values train the z scale.

- fatten:

  Median perimeter linewidth multiplier.

- shade:

  Whether to apply lighting.

- shape:

  Violin shape: `"wall"` or `"revolve"`.

- bw, adjust, trim:

  Density bandwidth, multiplier and trimming, as in
  [`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html).

- n:

  Number of density samples.

- sides:

  Number of sides of a revolved violin.

## Value

A ggplot2 layer.

## Examples

``` r
d <- data.frame(x = rep(c("a", "b"), each = 20), y = 0,
                z = c(seq(-1, 1, length.out = 20), seq(0, 3, length.out = 20)))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) +
  geom_boxplot_3d() + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) +
  geom_violin_3d(shape = "revolve", n = 24) + coord_3d()
```
