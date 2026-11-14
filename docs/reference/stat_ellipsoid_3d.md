# Covariance and confidence ellipsoids

Fits one ellipsoid to each group of points and draws it as a solid with
[`geom_ellipsoid_3d()`](https://gillescolling.com/ggplot3d/reference/geom_ellipsoid_3d.md):
the three-dimensional counterpart of
[`ggplot2::stat_ellipse()`](https://ggplot2.tidyverse.org/reference/stat_ellipse.html),
whose `type` and `level` it takes over. The ellipsoid is the unit sphere
carried through the square root of a shape matrix S onto the set of
points p with (p - centre)' S^-1 (p - centre) \<= 1, computed in the
scale-transformed units of the position axes.

## Usage

``` r
stat_ellipsoid_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomEllipsoid3D,
  position = "identity",
  ...,
  type = "t",
  level = 0.95,
  kind = "data",
  detail = 2L,
  axes = FALSE,
  shade = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
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

- geom:

  The geometry drawing the ellipsoids, by default the one of
  [`geom_ellipsoid_3d()`](https://gillescolling.com/ggplot3d/reference/geom_ellipsoid_3d.md).

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

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

- type:

  Centre and scatter estimate: `"t"`, `"norm"` or `"euclid"`.

- level:

  Confidence or coverage level, between zero and one; for
  `type = "euclid"` the radius of the sphere.

- kind:

  What the ellipsoid covers: `"data"`, `"se"` or `"ehull"`.

- detail:

  Subdivision level of the icosphere, from 0 to 6.

- axes:

  Whether to draw the three principal axes of each ellipsoid as opaque
  segments through its centre, in the `colour` aesthetic or, where that
  is missing, the `fill` darkened by half. Inside an opaque ellipsoid
  they are hidden; give the ellipsoids an `alpha` below one to see them.

- shade:

  Whether to apply lighting.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Value

A ggplot2 layer.

## Details

`type` chooses the centre and the scatter matrix V:

- `"t"` (the default) the location and scatter of a multivariate t
  distribution with five degrees of freedom, fitted by iterative
  reweighting as
  [`MASS::cov.trob()`](https://rdrr.io/pkg/MASS/man/cov.trob.html) does,
  which keeps outlying points from inflating the ellipsoid;

- `"norm"` the mean and the sample covariance;

- `"euclid"` the mean and a sphere of radius `level`, in data units.

`kind` chooses what the ellipsoid covers, with p = 3 dimensions and n
points in the group:

- `"data"` (the default) the spread of the observations, S = r^2 V with
  r^2 = p F(level; p, n - 1), the scaling of
  [`ggplot2::stat_ellipse()`](https://ggplot2.tidyverse.org/reference/stat_ellipse.html);

- `"se"` the confidence region of the centre, S = r^2 V / n with r^2 = p
  (n - 1) / (n - p) F(level; p, n - p), Hotelling's T^2 region for the
  mean of a normal sample. `vegan3d::orglellipse(kind = "se")` draws the
  same shape V / n scaled by a chi-square quantile, the large-sample
  limit of r^2;

- `"ehull"` the smallest ellipsoid that contains every point of the
  group, as `vegan3d::orglellipse(kind = "ehull")`, computed by
  Khachiyan's algorithm; `type` and `level` are not used.

A group needs at least four points that do not lie on one plane; other
groups are dropped with a message.

## Computed variables

- x, y, z:

  Centre of the ellipsoid.

- sigma:

  Shape matrix S, a list-column of 3 x 3 matrices.

- level:

  Confidence or coverage level; 1 for `kind = "ehull"`.

- volume:

  Volume of the ellipsoid, 4/3 pi sqrt(det S), in cubed
  scale-transformed units.

- n:

  Number of points in the group.

## References

Kent, Tyler and Vardi (1994). A curious likelihood identity for the
multivariate t-distribution. Communications in Statistics - Simulation
and Computation 23, 441-453.
[doi:10.1080/03610919408813180](https://doi.org/10.1080/03610919408813180)

Hotelling (1931). The generalization of Student's ratio. Annals of
Mathematical Statistics 2, 360-378.
[doi:10.1214/aoms/1177732979](https://doi.org/10.1214/aoms/1177732979)

Todd and Yildirim (2007). On Khachiyan's algorithm for the computation
of minimum-volume enclosing ellipsoids. Discrete Applied Mathematics
155, 1731-1744.
[doi:10.1016/j.dam.2007.02.013](https://doi.org/10.1016/j.dam.2007.02.013)

## See also

[`geom_ellipsoid_3d()`](https://gillescolling.com/ggplot3d/reference/geom_ellipsoid_3d.md)
for ellipsoids given by their axes or shape matrices.

## Examples

``` r
p <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width,
  z = Petal.Length, fill = Species))
p + geom_point_3d(ggplot2::aes(colour = Species)) +
  stat_ellipsoid_3d(alpha = 0.3) + coord_3d()

# Confidence regions of the three species means, with their principal axes.
p + geom_point_3d(ggplot2::aes(colour = Species), size = 0.6) +
  stat_ellipsoid_3d(kind = "se", type = "norm", alpha = 0.4, axes = TRUE) +
  coord_3d()

# The smallest ellipsoid around each species.
p + geom_point_3d(ggplot2::aes(colour = Species)) +
  stat_ellipsoid_3d(kind = "ehull", alpha = 0.2, detail = 3) + coord_3d()
```
