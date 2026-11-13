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
  panel_id = 1L
)
```

## Arguments

- vertices:

  List with x, y and z positions in normalised panel coordinates.

- corners:

  List with vertex indices and source_row input indices.

- faces, polylines, markers, text:

  Primitive tables. Each requires offset into corners. Faces default to
  three corners, polylines to two, and markers and text to one. Common
  columns include count, primitive_id, layer_id, source_row, style_id,
  role, partition and opacity. Markers also require footprint_id. Text
  sizes use width_mm and height_mm.

- footprints:

  Marker shapes: kind (1 for ellipse, 2 for polygon), offset, count,
  radius_x_mm, radius_y_mm and angle.

- footprint_vertices:

  Polygon footprint coordinates x_mm, y_mm and ring.

- styles:

  Data frame with contiguous integer style_id keys, fill, colour and
  other rendering properties. Face lighting uses shade and optional
  normal_x, normal_y, normal_z in normalised panel coordinates.

- panel_id:

  Positive integer panel identifier.

## Value

A validated ggplot3d_scene_packet containing geometry and styles.

## Examples

``` r
new_scene_packet()
```
