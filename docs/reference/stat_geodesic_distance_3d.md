# Geodesic distance, log map and shortest paths on a mesh

Stats over a mesh layer's data (a
[`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md)
corner table, an rgl `mesh3d`, or the corners a surface stat returns)
that measure along the surface. They run the [intrinsic
geometry](https://gillescolling.com/ggplot3d/reference/mesh_geodesic.md)
verbs on each panel, in transformed position units, so points are given
in the same units as the positions.

## Usage

``` r
stat_geodesic_distance_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomMesh3D,
  position = "identity",
  ...,
  source,
  time = 1,
  boundary = c("average", "neumann", "dirichlet"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_logmap_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomMesh3D,
  position = "identity",
  ...,
  source,
  direction = NULL,
  time = 1,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_geodesic_path_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomGeodesicPath3D,
  position = "identity",
  ...,
  from,
  to,
  max_iterations = 100000L,
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

  Geom drawing the result.

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

- source:

  Source points, `c(x, y, z)` or a matrix or data frame of points;
  [`mesh_logmap()`](https://gillescolling.com/ggplot3d/reference/mesh_geodesic.md)
  takes one point.

- time:

  Heat time as a multiple of the squared mean edge length. Larger values
  give a smoother distance.

- boundary:

  Heat at boundary vertices: `"average"`, `"neumann"` or `"dirichlet"`.

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

- direction:

  Tangent direction of the logarithmic map's angle zero, `c(x, y, z)`,
  or NULL for the first edge at the source.

- from, to:

  Path ends, as points; rows pair up.

- max_iterations:

  Largest number of FlipOut steps per path.

## Value

A ggplot2 layer.

## Details

`stat_geodesic_distance_3d()` computes `distance`, the geodesic distance
from the nearest of the `source` points, by the heat method.
`stat_logmap_3d()` computes `logmap_u` and `logmap_v`, polar coordinates
around `source` whose radius is the geodesic distance and whose angle is
that of the shortest geodesic, by the vector heat method; mapped to a
periodic fill they place a texture or decal on the surface. Both draw
the mesh, and their computed variables are averaged over each face, so a
fill mapped from them is constant per face; the verbs give the values at
the vertices.

`stat_geodesic_path_3d()` draws the shortest paths between the mesh
vertices closest to the rows of `from` and `to`, found by FlipOut, as
polylines lying on the surface. It computes `s`, the arc length along
each path, and `length`, the length of the whole path.

## Examples

``` r
sphere <- function(u, v) list(x = cos(u) * sin(v), y = sin(u) * sin(v), z = cos(v))
m <- ggplot2::layer_data(ggplot2::ggplot() +
  stat_parametric_3d(fun = sphere, v = c(0, pi), n = c(40, 21), closed = c(TRUE, FALSE)) +
  coord_3d())
m <- mesh_3d(unique(m[c("x", "y", "z", ".vertex")]),
  data.frame(.face = m$.face, .corner = m$.corner, .vertex = m$.vertex))
ggplot2::ggplot(m) +
  stat_geodesic_distance_3d(ggplot2::aes(fill = ggplot2::after_stat(distance)),
    source = c(0, 0, 1)) +
  stat_geodesic_path_3d(from = c(1, 0, 0), to = c(0, 0.6, 0.8), colour = "white") +
  coord_3d()
ggplot2::ggplot(m) +
  stat_logmap_3d(ggplot2::aes(fill = ggplot2::after_stat(
    (floor(4 * logmap_u) + floor(4 * logmap_v)) %% 2)),
    source = c(0, 0, 1), direction = c(1, 0, 0)) +
  coord_3d()
```
