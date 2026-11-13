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
  frame = "view"
)
```

## Arguments

- projection:

  Orthographic or perspective projection.

- azimuth, elevation, roll:

  Camera angles in degrees.

- distance, fov, zoom:

  Camera distance, field of view and magnification.

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

  Cube faces to draw.

- box:

  Box edges, back, all or none.

- z_scales:

  Fixed or free z scales across panels.

- crs:

  Target spatial coordinate reference system.

- light:

  Lighting configuration or NULL.

- backend:

  Backend configuration.

- default:

  Whether this is the default coordinate system.

- frame:

  Fit the current view, all azimuths at this elevation, or a sphere.

## Value

A Coord3D ggproto object.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_point_3d(size = 3) + coord_3d(azimuth = -45, elevation = 25)
d$site <- c("a", "a", "b")
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
  ggplot2::facet_wrap(~site) + coord_3d(z_scales = "free")
```
