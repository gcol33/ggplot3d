# Vector fields with solid cone heads

Maps x/y/z as anchors and u/v/w as vector components in original data
units. Endpoints are transformed by the position scales. Cone
orientation is calculated from the resulting endpoints after frame and
aspect mapping. Filled cone heads and tube shafts are analytic quadrics
(Sigg et al. 2006) with exact outlines and per-sample depth; heads
without a fill are drawn by the edges of a cone mesh of `detail`
subdivisions. `normalize = TRUE` makes the input vectors unit length
before applying `scale`. Computed variables `norm`, `xend`, `yend`,
`zend` are available through
[`after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.html).
Zero-length vectors draw no geometry.

## Usage

``` r
geom_quiver_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  scale = 1,
  normalize = FALSE,
  pivot = "tail",
  head = "cone",
  shaft = "line",
  head_length = 0.25,
  head_width = 0.1,
  shaft_width = 0.025,
  detail = 1L,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_cone_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  scale = 1,
  normalize = FALSE,
  pivot = "tail",
  head_width = 0.25,
  detail = 1L,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- scale:

  Nonnegative vector length multiplier.

- normalize:

  Whether to normalize the input vectors.

- pivot:

  Anchor location: `"tail"`, `"middle"` or `"tip"`.

- head:

  Head type: solid `"cone"`, a `"flat"` arrow, or `"none"`.

- shaft:

  Shaft geometry: `"line"`, `"tube"` or `"none"`.

- head_length, head_width:

  Head length and base radius as fractions of vector length.

- shaft_width:

  Tube radius as a fraction of vector length.

- detail:

  Glyph mesh subdivision, from 0 to 6.

- shade:

  Whether to apply lighting.

## Value

A ggplot2 layer.

## Examples

``` r
d <- expand.grid(x = -1:1, y = -1:1, z = 0:1)
d$u <- -d$y; d$v <- d$x; d$w <- 0.5
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w,
                               fill = ggplot2::after_stat(norm))) +
  geom_quiver_3d(scale = 0.4) + coord_3d()
```
