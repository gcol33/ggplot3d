# Exact three-dimensional panel coordinates

Set `z_scales = "free"` here to train a separate z range per facet.
ggplot2 facets control x and y scales and have no z scale.

## Usage

``` r
coord_3d(
  projection = "orthographic",
  azimuth = -60,
  elevation = 30,
  roll = 0,
  oblique = c(angle = 45, ratio = 0.5),
  up = "z",
  distance = NULL,
  fov = 35,
  zoom = 1,
  target = c(0.5, 0.5, 0.5),
  near = NULL,
  far = NULL,
  aspect = c(1, 1, 1),
  xlim = NULL,
  ylim = NULL,
  zlim = NULL,
  expand = TRUE,
  clip = "on",
  clip_data = TRUE,
  axes = c("x", "y", "z"),
  panes = c("back", "floor"),
  box = "back",
  z_scales = "fixed",
  crs = NULL,
  light = light_3d(),
  backend = backend_3d(),
  default = FALSE,
  frame = "view",
  transform = NULL,
  axis_lines = "box",
  origin = c(x = 0, y = 0, z = 0),
  clip_planes = list(),
  stereo = NULL,
  space = "euclidean",
  model = NULL,
  input = NULL,
  isometry = NULL,
  munch_tolerance = 0.001,
  munch_max_depth = 9L,
  shadow = NULL,
  depth_cue = NULL
)
```

## Arguments

- projection:

  `"orthographic"`, `"perspective"` or `"oblique"`. An oblique
  projection is parallel with a sight ray that is not perpendicular to
  the image: depth recedes on the page along `oblique`.

- azimuth, elevation, roll:

  Camera angles in degrees.

- oblique:

  Receding direction of an oblique projection: `angle` in degrees
  counterclockwise from the page's right, and `ratio`, the drawn length
  of one unit of depth.

- up:

  Vertical axis, `"x"`, `"y"` or `"z"`. Azimuth turns about it and
  elevation rises toward it.

- distance, fov, zoom:

  Camera distance, field of view and magnification. Each camera argument
  from `azimuth` to `zoom` takes one value, shared by all panels, or one
  value per panel in layout order, so facets can show the scene from
  different views.

- target:

  Target fractions within the expanded box.

- near, far:

  Camera clipping distances.

- aspect:

  Three positive model side lengths, or "data".

- xlim, ylim, zlim:

  Coordinate zoom limits in original units.

- expand:

  Logical scalar or named x, y, z logical vector.

- clip:

  Rectangular viewport clipping, on or off.

- clip_data:

  Whether to clip to the data box.

- axes:

  Axes to draw.

- panes:

  Cube faces to draw: `"back"` (the rear pane of each pair of walls),
  `"floor"`, `"front"` (the panes facing the viewer that are not drawn
  otherwise, styled by the theme element `panel.pane.front` and drawn
  over the data without grid lines), or pane names `"xmin"` to `"zmax"`.

- box:

  Box edges, back, all or none.

- z_scales:

  Fixed or free z scales across panels.

- crs:

  Target spatial coordinate reference system.

- light:

  Lighting from
  [`light_3d()`](https://gillescolling.com/ggplot3d/reference/light_3d.md)
  or
  [`lights_3d()`](https://gillescolling.com/ggplot3d/reference/lights_3d.md),
  one light such as
  [`light_point()`](https://gillescolling.com/ggplot3d/reference/light_directional.md),
  or NULL for none.

- backend:

  Backend configuration.

- default:

  Whether this is the default coordinate system.

- frame:

  Fit the current view, all azimuths at this elevation, or a sphere.

- transform:

  Optional coordinate transformations applied after statistics, as a
  named list with entries `x`, `y` or `z`, each a transformation name
  such as `"log10"` or a scales transformation. Lines and faces are
  subdivided so their images bend with the transformation.

- axis_lines:

  Where axis lines and their ticks and labels sit: `"box"` on an edge of
  the box, `"origin"` through the point `origin`, or `"none"`, which
  keeps ticks and labels on the box edges and draws no axis lines. A
  guide with an explicit position (see
  [`guide_axis_3d()`](https://gillescolling.com/ggplot3d/reference/guide_axis_3d.md))
  keeps it. Origin axes, their ticks, tick labels and titles are part of
  the scene, so data in front of them hide them; the label at the point
  where origin axes cross is left out. Origin axis lines use the theme
  elements `axis.line.origin.x`, `.y` and `.z`, which inherit from
  `axis.line.origin`.

- origin:

  Named numeric vector with entries `x`, `y` and `z` in data units, the
  point through which origin axes pass. Missing entries are 0. A value
  outside the panel range is moved to the nearest end of the range.

- clip_planes:

  A list of
  [`clip_plane_3d()`](https://gillescolling.com/ggplot3d/reference/clip_plane_3d.md)
  planes that cut every layer of the panel. A layer's own `clip_planes`
  argument replaces this list for that layer.

- stereo:

  Stereo output from
  [`stereo_3d()`](https://gillescolling.com/ggplot3d/reference/stereo_3d.md),
  or NULL for one view.

- space:

  Geometry of paths and faces: `"euclidean"` or an internal `Space3D`
  extension. Spaces define canonical points, chart conversions,
  distance, geodesics and isometries. See Details.

- model:

  Display model supplied by the space; NULL uses its default.

- input:

  Input chart supplied by the space; NULL uses `model`.

- isometry:

  Optional space-specific action on canonical points.

- munch_tolerance:

  Positive chord-error tolerance in projected panel fractions, used for
  geodesic subdivision. The default is 0.001.

- munch_max_depth:

  Maximum binary subdivision depth, from 0 to 12. A space reports an
  error when this budget cannot meet the tolerance.

- shadow:

  Planar cast shadows from
  [`shadow_3d()`](https://gillescolling.com/ggplot3d/reference/shadow_3d.md),
  or NULL for none.

- depth_cue:

  Fog, depth-thinned strokes and eye-dome lighting from
  [`depth_cue_3d()`](https://gillescolling.com/ggplot3d/reference/depth_cue_3d.md),
  or NULL for none.

## Value

A Coord3D ggproto object.

## Details

Space extensions inherit from `ggplot3d:::Space3D`. They declare
`charts`, `models` and `default_model`, and implement
`to_canonical(data, chart)`, `from_canonical(X, model)`,
`distance(X, Y)`, `geodesic(X, Y, s)` and `act(g, X)`. Canonical points
are finite numeric row matrices of any dimension. Geodesic endpoints are
paired rows and `s` is a scalar or one parameter per row, from zero to
one. Input positions are scale-transformed chart coordinates. Model
output has three columns in model axis units;
`model_ranges(model, panel_params)` supplies named x, y, z bounds.
Four-column homogeneous output (x, y, z, w) is also accepted and travels
unchanged into the scene packet; w = 0 is a point at infinity, and w \<
0 a point of the far half of oriented projective space (Stolfi 1991),
drawn only where the camera `pre` map brings it to w \>= 0. Projective
models such as the Klein ball return (X, X0), so clipping to the panel
box happens in homogeneous coordinates before division (Blinn and Newell
1978) and isometries stay linear. An edge whose image passes through
infinity carries its ideal point as an explicit corner.

`chart_is_projective(model)` declares straight geodesic images and
planar faces, allowing subdivision to be omitted. Other models share the
frame subdivision machinery, sampling geodesics before projection. Faces
use nested geodesic interpolation on a triangular lattice; shared edges
use identical canonical samples. Continuous paint attributes interpolate
by geodesic parameter, with piecewise constant paint on emitted
primitives. Discrete attributes and source identities follow the
starting corner. Polygon topology is triangulated in a planar input
chart; curved models receive those triangles and closed boundary
strokes. Panel furniture is supplied by the frame. Visibility is exact
for the resulting tessellation.

## Theme elements

Besides the ggplot2 elements, panels read these theme elements:

- `panel.pane`, `panel.pane.x`, `panel.pane.y`, `panel.pane.z`: pane
  fills, by the axis normal to the pane; `panel.pane.front` styles the
  panes selected with `panes = "front"`.

- `panel.grid.major.x.xy`, `panel.grid.minor.z.yz` and so on: the grid
  lines of one axis on the panes of one plane. Each inherits from the
  grid element of its axis (`panel.grid.major.x.xy` from
  `panel.grid.major.x`), so
  [`element_blank()`](https://ggplot2.tidyverse.org/reference/element.html)
  removes those lines from that plane only. In the default view the
  floor is the `xy` plane.

- `panel.zero.x`, `panel.zero.y`, `panel.zero.z`: a plane through the
  panel at transformed value 0 of that axis, when 0 lies inside the
  range. They are blank by default; setting one to an
  [`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html)
  draws it, filled from `panel.zero`. Zero planes are part of the scene,
  so they hide data behind them.

- `panel.zero.line.x`, `.y`, `.z`, inheriting from `panel.zero.line`
  (blank by default): the line at transformed value 0 on each drawn
  pane.

- `panel.box.back`, `panel.box.front`: box edges.

- `axis.line.origin.x`, `.y`, `.z`: origin axis lines.

- `panel.margin.3d`: the margin around the projected box.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_point_3d(size = 3) + coord_3d(azimuth = -45, elevation = 25)
d$site <- c("a", "a", "b")
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
  ggplot2::facet_wrap(~site) + coord_3d(z_scales = "free")
views <- rbind(transform(d, view = "front"), transform(d, view = "side"))
ggplot2::ggplot(views, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
  ggplot2::facet_wrap(~view) + coord_3d(azimuth = c(-60, 30))
centred <- data.frame(x = c(-2, 1, 2), y = c(1, -2, 2), z = c(-1, 2, 1))
ggplot2::ggplot(centred, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 3) +
  coord_3d(axis_lines = "origin", panes = "floor") +
  ggplot2::theme(panel.zero.z = ggplot2::element_rect(),
    panel.zero.line = ggplot2::element_line(colour = "grey30"))
ggplot2::ggplot(centred, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 3) +
  coord_3d(panes = c("back", "floor", "front")) +
  ggplot2::theme(panel.grid.major.z.xz = ggplot2::element_blank(),
    panel.pane.front = ggplot2::element_rect(fill = "#4682B41A"))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
  coord_3d(clip_planes = list(clip_plane_3d(c(0, 0, -1), c(z = 2.5))))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
  coord_3d(stereo = stereo_3d("cross"))
rotation <- diag(4)
rotation[1:2, 1:2] <- matrix(c(0, 1, -1, 0), 2)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_path_3d() +
  coord_3d(isometry = rotation, xlim = c(-4, 4), ylim = c(-4, 4))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 4) +
  coord_3d(shadow = shadow_3d(), depth_cue = depth_cue_3d())
```
