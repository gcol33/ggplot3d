# Point, path, segment, text and polygon layers in three dimensions

Point, path, segment, text and polygon layers in three dimensions

## Usage

``` r
geom_point_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_path_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_segment_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_text_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  orientation = "screen",
  plane = NULL,
  normal = NULL,
  up = NULL,
  facing = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_polygon_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- orientation:

  Text layers: `"screen"` draws labels upright on the page, `"plane"`
  draws font outline geometry in three dimensions.

- plane:

  Text plane spanned by `"xy"`, `"xz"` or `"yz"`; defaults to `"xy"`
  when neither `normal` nor `facing` is supplied.

- normal:

  A finite nonzero numeric triple, the plane normal in normalized panel
  coordinates. Supply one of `plane`, `normal` or `facing`.

- up:

  A finite nonzero reading-up hint in normalized panel coordinates. With
  `normal`, defaults to the z axis (y for a z normal). Camera-facing
  text defaults to the camera's reading-up direction. A parallel hint
  uses the least aligned coordinate axis to define the reading
  direction.

- facing:

  `"camera_plane"` gives labels a shared view-facing normal;
  `"camera_point"` faces each anchor toward the perspective eye.
  Parallel projections use the reversed sight ray, including oblique
  shear.

## Value

A ggplot2 layer emitting unprojected scene geometry.

## Details

Large point clouds: `geom_point_3d(render = "splat")` draws each point
in a raster backend as a sphere of the point's size, depth-tested per
pixel, and darkens points by eye-dome lighting, so an unlit cloud shows
its depth structure. `edl` (default 1, 0 for none) sets the lighting
strength. The vector backend draws splats as ordinary points and refuses
panels with more markers than the option `ggplot3d.vector_markers`
(default 50000).

Text in a plane: `geom_text_3d(orientation = "plane", plane = "xy")`
lays each label flat in the plane through its position spanned by two
axes (`"xy"`, `"xz"` or `"yz"`), read along the first axis and turned
within the plane by the `angle` aesthetic. The label is cut from the
glyph outlines of its font (read with the 'systemfonts' package) into
faces that the camera foreshortens and that hide and are hidden by the
data; `size` is the text size at the camera target. Seen from behind the
plane, a label reads in the turned direction instead of mirrored.

Normals and up hints refer to positions after scale normalization. The
frame's local Jacobian followed by aspect scaling transforms plane
tangents into displayed model space. Their cross product transforms
normals by the inverse transpose, preserving perpendicularity under
unequal aspect ratios and curved frames. Glyphs remain rigid in the
local tangent plane. `angle` rotates within that plane. Camera-facing
geometry is resolved for each panel at draw time in every static backend
and follows camera movement in the HTML viewer.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(1, 2, 4),
                label = letters[1:3])
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
p + geom_point_3d(size = 3) + coord_3d()
cloud <- data.frame(x = rnorm(2000), y = rnorm(2000), z = rnorm(2000))
ggplot2::ggplot(cloud, ggplot2::aes(x, y, z = z)) +
  geom_point_3d(render = "splat", size = 1) +
  coord_3d(backend = backend_3d("raster", dpi = 96))
p + geom_path_3d() + coord_3d()
p + geom_segment_3d(ggplot2::aes(xend = x, yend = y, zend = 0)) + coord_3d()
p + geom_text_3d(ggplot2::aes(label = label)) + coord_3d()
if (requireNamespace("systemfonts", quietly = TRUE)) {
  p + geom_text_3d(ggplot2::aes(label = label), orientation = "plane", plane = "xy", size = 8) +
    coord_3d(projection = "perspective")
  p + geom_text_3d(ggplot2::aes(label = label), orientation = "plane",
    normal = c(1, 1, 1), up = c(0, 0, 1)) + coord_3d()
  p + geom_text_3d(ggplot2::aes(label = label), orientation = "plane",
    facing = "camera_point") + coord_3d(projection = "perspective")
}
p + geom_polygon_3d(fill = "steelblue", alpha = 0.5) + coord_3d()
```
