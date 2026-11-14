# Stems from a base value to each point

Draws a line from each point to `base` along `axis`, with a marker at
the point, as `stem3` in MATLAB and `stem(orientation = )` in
matplotlib's 3D axes. Unlike
[`geom_dropline_3d()`](https://gillescolling.com/ggplot3d/reference/geom_dropline_3d.md),
which ends on a face of the box, a stem ends at a data value, so
positive and negative values point away from the base in opposite
directions.

## Usage

``` r
geom_stem_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  base = 0,
  axis = "z",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments. Mapping `zend` (or `xend`, `yend` for those
  axes) gives each stem its own base.

- base:

  Base value in original data units of `axis`.

- axis:

  Axis the stems run along: `"z"`, `"x"` or `"y"`.

## Value

A ggplot2 layer. Set `shape = NA` to draw the stems alone.

## Examples

``` r
d <- expand.grid(x = 1:5, y = 1:3)
d$z <- sin(d$x) * d$y
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, colour = z > 0)) +
  geom_stem_3d() + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_stem_3d(axis = "x", base = 0, shape = 21, fill = "white") + coord_3d()
```
