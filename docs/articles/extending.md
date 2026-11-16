# Writing a geometry extension

Every layer drawn in
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
hands its geometry to the panel’s scene as a scene packet: typed tables
of vertices, faces, polylines, markers and text. The scene merges the
packets of all layers and resolves visibility across them, so a new geom
only describes its geometry and never draws anything itself. This
article writes a geom that draws one triangle per group of three rows.
It assumes familiarity with
[ggproto](https://ggplot2-book.org/ext-springs.html) extensions; [How
overlapping geometry is
drawn](https://gillescolling.com/ggplot3d/articles/visibility.md)
explains what the scene does with the packets.

1.  **Define** the geom with
    [`ggplot2::ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html).

2.  **Emit** its geometry as a packet with
    [`new_scene_packet()`](https://gillescolling.com/ggplot3d/reference/new_scene_packet.md).

3.  **Register** the emitter with
    [`register_geom_3d()`](https://gillescolling.com/ggplot3d/reference/register_geom_3d.md).

4.  **Add** the layer to a plot with
    [`layer_3d()`](https://gillescolling.com/ggplot3d/reference/layer_3d.md).

``` r

library(ggplot2)
library(ggplot3d)
```

## The scene packet

A packet holds columnar tables. `vertices` gives positions, `corners`
lists the vertex of each face corner in order, `faces` says where each
face starts in the corner table (`offset`) and how many corners it has
(`count`), and `styles` holds the fill and outline colours that faces
refer to by `style_id`. Indices start at 1. An empty packet shows every
table with its columns.

``` r

new_scene_packet()
#> $schema_version
#> [1] 2
#> 
#> $panel_id
#> [1] 1
#> 
#> $style_count
#> [1] 0
#> 
#> $vertices
#> $vertices$x
#> numeric(0)
#> 
#> $vertices$y
#> numeric(0)
#> 
#> $vertices$z
#> numeric(0)
#> 
#> $vertices$w
#> numeric(0)
#> 
#> 
#> $corners
#> $corners$vertex
#> integer(0)
#> 
#> $corners$source_row
#> integer(0)
#> 
#> 
#> $faces
#> $faces$primitive_id
#> integer(0)
#> 
#> $faces$layer_id
#> integer(0)
#> 
#> $faces$source_row
#> integer(0)
#> 
#> $faces$style_id
#> integer(0)
#> 
#> $faces$offset
#> integer(0)
#> 
#> $faces$count
#> integer(0)
#> 
#> $faces$role
#> integer(0)
#> 
#> $faces$partition
#> integer(0)
#> 
#> $faces$opacity
#> numeric(0)
#> 
#> 
#> $polylines
#> $polylines$primitive_id
#> integer(0)
#> 
#> $polylines$layer_id
#> integer(0)
#> 
#> $polylines$source_row
#> integer(0)
#> 
#> $polylines$style_id
#> integer(0)
#> 
#> $polylines$offset
#> integer(0)
#> 
#> $polylines$count
#> integer(0)
#> 
#> $polylines$role
#> integer(0)
#> 
#> $polylines$partition
#> integer(0)
#> 
#> $polylines$opacity
#> numeric(0)
#> 
#> $polylines$width_mm
#> numeric(0)
#> 
#> $polylines$mitre_limit
#> numeric(0)
#> 
#> $polylines$cap
#> integer(0)
#> 
#> $polylines$join
#> integer(0)
#> 
#> $polylines$hidden
#> integer(0)
#> 
#> $polylines$arrow_start
#> logical(0)
#> 
#> $polylines$arrow_end
#> logical(0)
#> 
#> $polylines$mask_width_mm
#> numeric(0)
#> 
#> 
#> $markers
#> $markers$primitive_id
#> integer(0)
#> 
#> $markers$layer_id
#> integer(0)
#> 
#> $markers$source_row
#> integer(0)
#> 
#> $markers$style_id
#> integer(0)
#> 
#> $markers$offset
#> integer(0)
#> 
#> $markers$count
#> integer(0)
#> 
#> $markers$role
#> integer(0)
#> 
#> $markers$partition
#> integer(0)
#> 
#> $markers$opacity
#> numeric(0)
#> 
#> $markers$footprint_id
#> integer(0)
#> 
#> 
#> $text
#> $text$primitive_id
#> integer(0)
#> 
#> $text$layer_id
#> integer(0)
#> 
#> $text$source_row
#> integer(0)
#> 
#> $text$style_id
#> integer(0)
#> 
#> $text$offset
#> integer(0)
#> 
#> $text$count
#> integer(0)
#> 
#> $text$role
#> integer(0)
#> 
#> $text$partition
#> integer(0)
#> 
#> $text$opacity
#> numeric(0)
#> 
#> $text$width_mm
#> numeric(0)
#> 
#> $text$height_mm
#> numeric(0)
#> 
#> $text$hjust
#> numeric(0)
#> 
#> $text$vjust
#> numeric(0)
#> 
#> $text$angle
#> numeric(0)
#> 
#> 
#> $quadrics
#> $quadrics$primitive_id
#> integer(0)
#> 
#> $quadrics$layer_id
#> integer(0)
#> 
#> $quadrics$source_row
#> integer(0)
#> 
#> $quadrics$style_id
#> integer(0)
#> 
#> $quadrics$partition
#> integer(0)
#> 
#> $quadrics$opacity
#> numeric(0)
#> 
#> $quadrics$kind
#> integer(0)
#> 
#> $quadrics$transform_1
#> numeric(0)
#> 
#> $quadrics$transform_2
#> numeric(0)
#> 
#> $quadrics$transform_3
#> numeric(0)
#> 
#> $quadrics$transform_4
#> numeric(0)
#> 
#> $quadrics$transform_5
#> numeric(0)
#> 
#> $quadrics$transform_6
#> numeric(0)
#> 
#> $quadrics$transform_7
#> numeric(0)
#> 
#> $quadrics$transform_8
#> numeric(0)
#> 
#> $quadrics$transform_9
#> numeric(0)
#> 
#> $quadrics$transform_10
#> numeric(0)
#> 
#> $quadrics$transform_11
#> numeric(0)
#> 
#> $quadrics$transform_12
#> numeric(0)
#> 
#> $quadrics$transform_13
#> numeric(0)
#> 
#> $quadrics$transform_14
#> numeric(0)
#> 
#> $quadrics$transform_15
#> numeric(0)
#> 
#> $quadrics$transform_16
#> numeric(0)
#> 
#> $quadrics$radius_ratio
#> numeric(0)
#> 
#> $quadrics$cap_0
#> integer(0)
#> 
#> $quadrics$cap_1
#> integer(0)
#> 
#> 
#> $footprints
#> $footprints$kind
#> integer(0)
#> 
#> $footprints$offset
#> integer(0)
#> 
#> $footprints$count
#> integer(0)
#> 
#> $footprints$radius_x_mm
#> numeric(0)
#> 
#> $footprints$radius_y_mm
#> numeric(0)
#> 
#> $footprints$angle
#> numeric(0)
#> 
#> 
#> $footprint_vertices
#> $footprint_vertices$x_mm
#> numeric(0)
#> 
#> $footprint_vertices$y_mm
#> numeric(0)
#> 
#> $footprint_vertices$ring
#> integer(0)
#> 
#> 
#> $styles
#> [1] style_id
#> <0 rows> (or 0-length row.names)
#> 
#> $materials
#> [1] material_id
#> <0 rows> (or 0-length row.names)
#> 
#> attr(,"class")
#> [1] "ggplot3d_scene_packet"
```

Positions in a packet are normalised to the panel box, which runs from 0
to 1 along each axis.
[`normalise_homogeneous_3d()`](https://gillescolling.com/ggplot3d/reference/normalise_homogeneous_3d.md)
does that conversion from data units given the panel ranges. A fourth
homogeneous coordinate `w = 0` marks a point at infinity in a direction,
which the engine clips to the box; the second point below is the
direction of the positive x axis.

``` r

normalise_homogeneous_3d(c(5, 1), c(0, 0), c(1, 0), w = c(1, 0),
  ranges = list(x = c(0, 10), y = c(-1, 1), z = c(0, 2)))
#> $x
#> [1] 0.5 0.1
#> 
#> $y
#> [1] 0.5 0.0
#> 
#> $z
#> [1] 0.5 0.0
#> 
#> $w
#> [1] 1 0
```

## An emitter

The geom itself declares its aesthetics like any ggplot2 geom. It needs
no `draw_panel()` method, because
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
never calls one for registered geoms.

``` r

GeomTriangle3D <- ggproto("GeomTriangle3D", Geom,
  required_aes = c("x", "y", "z"),
  default_aes = aes(fill = "#2E6F9E", alpha = 1),
  draw_key = draw_key_polygon)
```

The emitter receives the layer’s data after scales and statistics, with
x, y and z already normalised to the panel box, plus two internal
columns: `.source`, the input row of each record, and `.layer`, the
layer index. It returns one packet. Writing `source_row` and `layer_id`
into the packet lets the viewer and the alt text trace each face back to
its data.

``` r

emit_triangle <- function(data, params, panel_params) {
  stopifnot(nrow(data) == 3L)
  new_scene_packet(
    vertices = list(x = as.double(data$x), y = as.double(data$y),
                    z = as.double(data$z)),
    corners = list(vertex = 1:3, source_row = as.integer(data$.source)),
    faces = list(offset = 1L, count = 3L, style_id = 1L,
                 source_row = as.integer(data$.source[1]),
                 layer_id = as.integer(data$.layer[1]), opacity = data$alpha[1]),
    styles = data.frame(style_id = 1L, fill = data$fill[1],
                        colour = NA_character_, shade = FALSE),
    panel_id = panel_params$panel_id)
}
```

## Registering and using the geom

[`register_geom_3d()`](https://gillescolling.com/ggplot3d/reference/register_geom_3d.md)
maps the geom class to its emitter. From then on any layer with that
geom works in
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md);
[`layer_3d()`](https://gillescolling.com/ggplot3d/reference/layer_3d.md)
adds a ggplot2 layer object to the plot.

``` r

register_geom_3d(GeomTriangle3D, emit_triangle, replace = TRUE)
triangle <- layer(geom = GeomTriangle3D, stat = "identity",
                  position = "identity", params = list())
corners <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 1, 1))
ggplot(corners, aes(x, y, z = z)) + layer_3d(triangle) + coord_3d()
```

![A single blue triangle drawn by the new geom inside the panel
box.](extending_files/figure-html/register-1.svg)

The triangle depth-sorts with the panel’s grid lines and back walls like
any built-in layer, and a second layer that crosses it would be split
where the two intersect.

## Where next

- [How overlapping geometry is
  drawn](https://gillescolling.com/ggplot3d/articles/visibility.md): how
  packets from all layers are ordered and split.

- [Reading and measuring
  meshes](https://gillescolling.com/ggplot3d/articles/meshes.md): build
  mesh data that
  [`geom_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/geom_mesh_3d.md)
  draws without a new geom.
