# Schwarzschild spatial embedding diagram

Flamm's paraboloid has height 2 sqrt(rs (r-rs)), where rs=2M in G=c=1
units. It embeds the t=constant, theta=pi/2 spatial slice. Spacetime
orbits drawn on it are projections. Its intrinsic surface geodesics are
available through
[`stat_parametric_geodesic_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_geodesic_3d.md).
A square-root radial parameter keeps the throat regular. Set both=TRUE
for both sheets of the bridge.

## Usage

``` r
stat_embedding_3d(
  mapping = NULL,
  data = NULL,
  ...,
  metric = "schwarzschild",
  rs = 1,
  r = c(rs, 4 * rs),
  both = FALSE,
  n = 40L
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- metric:

  Spatial metric, currently "schwarzschild".

- rs:

  Positive Schwarzschild radius.

- r:

  Radial range, beginning at or outside rs.

- both:

  Draw both sheets.

- n:

  Number of samples along u and v, scalar or two integers.

## Value

A surface layer or a list of two surface layers.

## References

Sadegh, Kiani and Nouri-Zonoz (2024). Embedding diagrams in stationary
spacetimes. <https://arxiv.org/abs/2308.03104>.

## Examples

``` r
ggplot2::ggplot()+stat_embedding_3d(rs=1,n=c(24,12))+coord_3d()
```
