# Construct a 3D geometry packet

Emitters registered with
[`register_geom_3d()`](https://gillescolling.com/ggplot3d/reference/register_geom_3d.md)
return a validated scene packet. Each geometry table is a named list of
equal-length numeric vectors. Indices start at one. Empty lists
represent unused geometry kinds.

## Usage

``` r
new_scene_packet(
  vertices = list(),
  corners = list(),
  faces = list(),
  polylines = list(),
  markers = list(),
  text = list(),
  footprints = list(),
  footprint_vertices = list(),
  styles = data.frame(style_id = integer()),
  panel_id = 1L,
  quadrics = list(),
  materials = data.frame(material_id = integer())
)
```

## Arguments

- vertices:

  List with x, y, z and optional w: homogeneous positions in normalised
  panel coordinates, see
  [`normalise_homogeneous_3d()`](https://gillescolling.com/ggplot3d/reference/normalise_homogeneous_3d.md).
  `w` defaults to 1 and may take either sign: rows are points of
  oriented projective space (Stolfi 1991), so (x, y, z, w) and (-x, -y,
  -z, -w) are distinct points, and `w = 0` places an ideal point (a
  direction) at infinity. Only the half `w >= 0` after the camera `pre`
  map is drawn. Geometry through an ideal point is clipped by the panel
  box, the near and far planes or the viewport before it is drawn.

- corners:

  List with vertex indices and source_row input indices, and optional
  per-corner attributes normal_x, normal_y, normal_z, red, green, blue,
  alpha (encoded sRGB colour channels in \[0, 1\], interpolated in
  linear light) and u, v (texture coordinates), in that order; NA marks
  a corner without the attribute. Compiled vertex weights carry them
  through clipping.

- faces, polylines, markers, text:

  Primitive tables. Each requires offset into corners. Faces default to
  three corners, polylines to two, and markers and text to one. Common
  columns include count, primitive_id, layer_id, source_row, style_id,
  role, partition and opacity. Markers also require footprint_id. Faces
  may carry logical coverage_opaque (default TRUE): whether their entire
  area is opaque when opacity is 1. Text sizes use width_mm and
  height_mm. Polylines take an optional mask_width_mm, at least width_mm
  and by default equal to it: the width of the band in which the line
  hides what lies behind it, while the line itself is drawn at width_mm
  (a haloed line, Appel 1967).

- footprints:

  Marker shapes: kind (1 for ellipse, 2 for polygon), offset, count,
  radius_x_mm, radius_y_mm and angle.

- footprint_vertices:

  Polygon footprint coordinates x_mm, y_mm and ring.

- styles:

  Data frame with contiguous integer style_id keys, fill, colour and
  other rendering properties. Face lighting uses shade and optional
  normal_x, normal_y, normal_z in normalised panel coordinates. An
  optional integer texture_id (NA for none) names a texture held on the
  R side.

- panel_id:

  Positive integer panel identifier.

- quadrics:

  Analytic primitives, each a unit primitive of `kind` (1 sphere, 2
  cylinder, 3 cone) placed by an affine map into normalised panel
  coordinates: `transform_1` to `transform_16`, the column-major entries
  of a 4 x 4 matrix whose last row is (0, 0, 0, 1), identity by default
  (Sigg et al. 2006). The unit sphere is the ball of radius 1 about the
  origin; unit cylinders and cones lie along the local z axis for z in
  \[0, 1\], with radius 1 at z = 0 and `radius_ratio` (default 1,
  exactly 1 for cylinders) at z = 1. `cap_0` and `cap_1` close the ends
  at z = 0 and z = 1 with flat disks (1, the default) or leave them open
  (0). Common columns are primitive_id, layer_id, source_row, style_id,
  partition and opacity.

- materials:

  Data frame of surface materials with contiguous integer material_id
  keys and columns ambient, diffuse, specular, shininess,
  specular_colour and back (a colour or NA), as made by
  [`material_3d()`](https://gillescolling.com/ggplot3d/reference/material_3d.md).
  A style's optional integer material_id (NA for the default material)
  refers to it. The engine reads neither; the shading kernel does.

## Value

A validated ggplot3d_scene_packet containing geometry and styles.

## Examples

``` r
new_scene_packet()
# A triangle with one corner at infinity in the x direction.
new_scene_packet(
  vertices = list(x = c(0.2, 0.2, 1), y = c(0.2, 0.8, 0), z = c(0.5, 0.5, 0),
                  w = c(1, 1, 0)),
  corners = list(vertex = 1:3),
  faces = list(offset = 1L),
  styles = data.frame(style_id = 1L, fill = "grey60"))
```
