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

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iNDY4LjAwcHQiIGhlaWdodD0iMzI0LjAwcHQiIHZpZXdib3g9IjAgMCA0NjguMDAgMzI0LjAwIiBjbGFzcz0ici1wbHQgaW5saW5lLXN2ZyIgc3R5bGU9IndpZHRoOjEwMCU7aGVpZ2h0OmF1dG87IiByb2xlPSJpbWciIGFyaWEtbGFiZWw9IkEgc2luZ2xlIGJsdWUgdHJpYW5nbGUgZHJhd24gYnkgdGhlIG5ldyBnZW9tIGluc2lkZSB0aGUgcGFuZWwgYm94LiI+PHRpdGxlPkEgc2luZ2xlIGJsdWUgdHJpYW5nbGUgZHJhd24gYnkgdGhlIG5ldyBnZW9tIGluc2lkZSB0aGUgcGFuZWwgYm94LjwvdGl0bGU+CjxnIGNsYXNzPSJzdmdsaXRlIj48ZGVmcz48c3R5bGUgdHlwZT0idGV4dC9jc3MiPgogICAgLyogdGhlbWUtYXdhcmUtcHJvY2Vzc2VkICovCiAgICAuc3ZnbGl0ZSBsaW5lLCAuc3ZnbGl0ZSBwb2x5bGluZSwgLnN2Z2xpdGUgcG9seWdvbiwgLnN2Z2xpdGUgcGF0aCwgLnN2Z2xpdGUgY2lyY2xlIHsKICAgICAgZmlsbDogbm9uZTsKICAgICAgc3Ryb2tlOiAjM0UzRjNBOwogICAgICBzdHJva2UtbGluZWNhcDogcm91bmQ7CiAgICAgIHN0cm9rZS1saW5lam9pbjogcm91bmQ7CiAgICAgIHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsKICAgIH0KICAgIC5zdmdsaXRlID4gZyA+IHJlY3QgewogICAgICBmaWxsOiBub25lOwogICAgICBzdHJva2U6ICMzRTNGM0E7CiAgICAgIHN0cm9rZS1saW5lY2FwOiByb3VuZDsKICAgICAgc3Ryb2tlLWxpbmVqb2luOiByb3VuZDsKICAgICAgc3Ryb2tlLW1pdGVybGltaXQ6IDEwLjAwOwogICAgfQogICAgLnN2Z2xpdGUgdGV4dCB7CiAgICAgIHdoaXRlLXNwYWNlOiBwcmU7CiAgICAgIGZpbGw6ICMzRTNGM0E7CiAgICB9CiAgICAuc3ZnbGl0ZSBnLmdseXBoZ3JvdXAgcGF0aCB7CiAgICAgIGZpbGw6IGluaGVyaXQ7CiAgICAgIHN0cm9rZTogbm9uZTsKICAgIH0KICA8L3N0eWxlPjwvZGVmcz48cmVjdCBjbGFzcz0ic3ZnLWJnIiB3aWR0aD0iMTAwJSIgaGVpZ2h0PSIxMDAlIiBzdHlsZT0ic3Ryb2tlOiBub25lOyBmaWxsOiAjRjVGNkY4OyIgLz48ZGVmcz48Y2xpcHBhdGggaWQ9ImYyLWNwTUM0d01IdzBOamd1TURCOE1DNHdNSHd6TWpRdU1EQT0iPjxyZWN0IHg9IjAuMDAiIHk9IjAuMDAiIHdpZHRoPSI0NjguMDAiIGhlaWdodD0iMzI0LjAwIiAvPjwvY2xpcHBhdGg+PC9kZWZzPjxnIGNsaXAtcGF0aD0idXJsKCNmMi1jcE1DNHdNSHcwTmpndU1EQjhNQzR3TUh3ek1qUXVNREE9KSI+PC9nPjxkZWZzPjxjbGlwcGF0aCBpZD0iZjItY3BOekl1TURCOE16azJMakF3ZkRBdU1EQjhNekkwTGpBdyI+PHJlY3QgeD0iNzIuMDAiIHk9IjAuMDAiIHdpZHRoPSIzMjQuMDAiIGhlaWdodD0iMzI0LjAwIiAvPjwvY2xpcHBhdGg+PC9kZWZzPjxnIGNsaXAtcGF0aD0idXJsKCNmMi1jcE56SXVNREI4TXprMkxqQXdmREF1TURCOE16STBMakF3KSI+PHJlY3QgeD0iNzIuMDAiIHk9IjAuMDAiIHdpZHRoPSIzMjQuMDAiIGhlaWdodD0iMzI0LjAwIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjY1OyBzdHJva2U6IG5vbmU7IiAvPjwvZz48ZyBjbGlwLXBhdGg9InVybCgjZjItY3BNQzR3TUh3ME5qZ3VNREI4TUM0d01Id3pNalF1TURBPSkiPjxyZWN0IHg9IjgwLjQ3IiB5PSI4LjQ3IiB3aWR0aD0iMzA3LjA2IiBoZWlnaHQ9IjMwNy4wNiIgc3R5bGU9InN0cm9rZS13aWR0aDogMS42NTsgc3Ryb2tlOiBub25lOyIgLz48L2c+PGRlZnM+PGNsaXBwYXRoIGlkPSJmMi1jcE1UTTNMakU0ZkRNMU9DNDNNWHd4Tmk0ME5Id3pNRFV1TmpBPSI+PHJlY3QgeD0iMTM3LjE4IiB5PSIxNi40NCIgd2lkdGg9IjIyMS41MyIgaGVpZ2h0PSIyODkuMTYiIC8+PC9jbGlwcGF0aD48L2RlZnM+PGcgY2xpcC1wYXRoPSJ1cmwoI2YyLWNwTVRNM0xqRTRmRE0xT0M0M01Yd3hOaTQwTkh3ek1EVXVOakE9KSI+PHBvbHlsaW5lIHBvaW50cz0iMTQxLjM1LDI0Mi41MiAxNDEuMzUsMTAyLjcxICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMTUwLjUyLDIzNC41OCAxNTAuNTIsOTQuNzcgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjEwOyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIxNTkuNzAsMjI2LjYzIDE1OS43MCw4Ni44MiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE2OC44NywyMTguNjkgMTY4Ljg3LDc4Ljg4ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMTc4LjA0LDIxMC43NCAxNzguMDQsNzAuOTMgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjEwOyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIxODcuMjIsMjAyLjgwIDE4Ny4yMiw2Mi45OSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE5Ni4zOSwxOTQuODYgMTk2LjM5LDU1LjA1ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMjA1LjU2LDE4Ni45MSAyMDUuNTYsNDcuMTAgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjEwOyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIyMTQuNzMsMTc4Ljk3IDIxNC43MywzOS4xNiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE0MS4zNSwyNDIuNTIgMTQxLjM1LDEwMi43MSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE1OS43MCwyMjYuNjMgMTU5LjcwLDg2LjgyICIgc3R5bGU9InN0cm9rZS13aWR0aDogMi4xOTsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMTc4LjA0LDIxMC43NCAxNzguMDQsNzAuOTMgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAyLjE5OyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIxOTYuMzksMTk0Ljg2IDE5Ni4zOSw1NS4wNSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxNC43MywxNzguOTcgMjE0LjczLDM5LjE2ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMi4xOTsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMTM3LjY4LDIzOS4zNCAyMTguNDAsMTY5LjQ0ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMTM3LjY4LDIyMy40NSAyMTguNDAsMTUzLjU1ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMTM3LjY4LDIwNy41NyAyMTguNDAsMTM3LjY2ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMTM3LjY4LDE5MS42OCAyMTguNDAsMTIxLjc3ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMTM3LjY4LDE3NS43OSAyMTguNDAsMTA1Ljg5ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMTM3LjY4LDE1OS45MCAyMTguNDAsOTAuMDAgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjEwOyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIxMzcuNjgsMTQ0LjAyIDIxOC40MCw3NC4xMSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjEzNy42OCwxMjguMTMgMjE4LjQwLDU4LjIyICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMTM3LjY4LDExMi4yNCAyMTguNDAsNDIuMzQgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjEwOyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIxMzcuNjgsMjM5LjM0IDIxOC40MCwxNjkuNDQgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAyLjE5OyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIxMzcuNjgsMjA3LjU3IDIxOC40MCwxMzcuNjYgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAyLjE5OyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIxMzcuNjgsMTc1Ljc5IDIxOC40MCwxMDUuODkgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAyLjE5OyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIxMzcuNjgsMTQ0LjAyIDIxOC40MCw3NC4xMSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjEzNy42OCwxMTIuMjQgMjE4LjQwLDQyLjM0ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMi4xOTsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMjI0Ljc2LDE3Ny42MyAyMjQuNzYsMzcuODIgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjEwOyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIyNDAuNjUsMTgyLjIxIDI0MC42NSw0Mi40MCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjI1Ni41MywxODYuODAgMjU2LjUzLDQ2Ljk5ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMjcyLjQyLDE5MS4zOSAyNzIuNDIsNTEuNTcgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjEwOyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIyODguMzEsMTk1Ljk3IDI4OC4zMSw1Ni4xNiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjMwNC4yMCwyMDAuNTYgMzA0LjIwLDYwLjc1ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMzIwLjA4LDIwNS4xNCAzMjAuMDgsNjUuMzMgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjEwOyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIzMzUuOTcsMjA5LjczIDMzNS45Nyw2OS45MiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjM1MS44NiwyMTQuMzIgMzUxLjg2LDc0LjUxICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMjI0Ljc2LDE3Ny42MyAyMjQuNzYsMzcuODIgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAyLjE5OyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIyNTYuNTMsMTg2LjgwIDI1Ni41Myw0Ni45OSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjI4OC4zMSwxOTUuOTcgMjg4LjMxLDU2LjE2ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMi4xOTsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMzIwLjA4LDIwNS4xNCAzMjAuMDgsNjUuMzMgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAyLjE5OyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIzNTEuODYsMjE0LjMyIDM1MS44Niw3NC41MSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCwxNjkuNDQgMzU4LjIxLDIwOS44MCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCwxNTMuNTUgMzU4LjIxLDE5My45MSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCwxMzcuNjYgMzU4LjIxLDE3OC4wMiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCwxMjEuNzcgMzU4LjIxLDE2Mi4xMyAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCwxMDUuODkgMzU4LjIxLDE0Ni4yNSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCw5MC4wMCAzNTguMjEsMTMwLjM2ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMjE4LjQwLDc0LjExIDM1OC4yMSwxMTQuNDcgIiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjEwOyBzdHJva2U6ICNERkQ3Q0E7IHN0cm9rZS1saW5lY2FwOiBidXR0OyI+PC9wb2x5bGluZT48cG9seWxpbmUgcG9pbnRzPSIyMTguNDAsNTguMjIgMzU4LjIxLDk4LjU4ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4xMDsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMjE4LjQwLDQyLjM0IDM1OC4yMSw4Mi43MCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCwxNjkuNDQgMzU4LjIxLDIwOS44MCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCwxMzcuNjYgMzU4LjIxLDE3OC4wMiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCwxMDUuODkgMzU4LjIxLDE0Ni4yNSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCw3NC4xMSAzNTguMjEsMTE0LjQ3ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMi4xOTsgc3Ryb2tlOiAjREZEN0NBOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlsaW5lIHBvaW50cz0iMjE4LjQwLDQyLjM0IDM1OC4yMSw4Mi43MCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE0NC4wNCwyNDcuNTMgMjI0Ljc2LDE3Ny42MyAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE1OS45MywyNTIuMTIgMjQwLjY1LDE4Mi4yMSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE3NS44MSwyNTYuNzAgMjU2LjUzLDE4Ni44MCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE5MS43MCwyNjEuMjkgMjcyLjQyLDE5MS4zOSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIwNy41OSwyNjUuODggMjg4LjMxLDE5NS45NyAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIyMy40OCwyNzAuNDYgMzA0LjIwLDIwMC41NiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIzOS4zNiwyNzUuMDUgMzIwLjA4LDIwNS4xNCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjI1NS4yNSwyNzkuNjQgMzM1Ljk3LDIwOS43MyAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjI3MS4xNCwyODQuMjIgMzUxLjg2LDIxNC4zMiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE0NC4wNCwyNDcuNTMgMjI0Ljc2LDE3Ny42MyAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE3NS44MSwyNTYuNzAgMjU2LjUzLDE4Ni44MCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIwNy41OSwyNjUuODggMjg4LjMxLDE5NS45NyAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIzOS4zNiwyNzUuMDUgMzIwLjA4LDIwNS4xNCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjI3MS4xNCwyODQuMjIgMzUxLjg2LDIxNC4zMiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE0MS4zNSwyNDIuNTIgMjgxLjE2LDI4Mi44OCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE1MC41MiwyMzQuNTggMjkwLjM0LDI3NC45NCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE1OS43MCwyMjYuNjMgMjk5LjUxLDI2Ni45OSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE2OC44NywyMTguNjkgMzA4LjY4LDI1OS4wNSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE3OC4wNCwyMTAuNzQgMzE3Ljg1LDI1MS4xMCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE4Ny4yMiwyMDIuODAgMzI3LjAzLDI0My4xNiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE5Ni4zOSwxOTQuODYgMzM2LjIwLDIzNS4yMiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIwNS41NiwxODYuOTEgMzQ1LjM3LDIyNy4yNyAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxNC43MywxNzguOTcgMzU0LjU0LDIxOS4zMyAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDEuMTA7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE0MS4zNSwyNDIuNTIgMjgxLjE2LDI4Mi44OCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE1OS43MCwyMjYuNjMgMjk5LjUxLDI2Ni45OSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE3OC4wNCwyMTAuNzQgMzE3Ljg1LDI1MS4xMCAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjE5Ni4zOSwxOTQuODYgMzM2LjIwLDIzNS4yMiAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxNC43MywxNzguOTcgMzU0LjU0LDIxOS4zMyAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDIuMTk7IHN0cm9rZTogI0RGRDdDQTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCwxNzUuNzkgMzU4LjIxLDIxNi4xNSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDAuODU7IHN0cm9rZTogIzdGN0Y3Rjsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjEzNy42OCwyNDUuNzAgMjE4LjQwLDE3NS43OSAiIHN0eWxlPSJzdHJva2Utd2lkdGg6IDAuODU7IHN0cm9rZTogIzdGN0Y3Rjsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7Ij48L3BvbHlsaW5lPjxwb2x5bGluZSBwb2ludHM9IjIxOC40MCwxNzUuNzkgMjE4LjQwLDM1Ljk4ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMC44NTsgc3Ryb2tlOiAjN0Y3RjdGOyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvcG9seWxpbmU+PHBvbHlnb24gcG9pbnRzPSIxNDcuNzEsMjM4LjAwIDI3NC44MSwxNDcuNTkgMjIxLjA5LDQ3LjM1ICIgc3R5bGU9InN0cm9rZS13aWR0aDogMS4wNzsgc3Ryb2tlOiBub25lOyBmaWxsOiAjMkU2RjlFOyI+PC9wb2x5Z29uPjwvZz48ZyBjbGlwLXBhdGg9InVybCgjZjItY3BNQzR3TUh3ME5qZ3VNREI4TUM0d01Id3pNalF1TURBPSkiPjx0ZXh0IHg9IjE0MC4xNSIgeT0iMjY1Ljg1IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBzdHlsZT0iZm9udC1zaXplOiAxLjM2ZW07ZmlsbDogIzRENEQ0RDsgZm9udC1mYW1pbHk6ICZxdW90O1JvYm90byZxdW90OywgLWFwcGxlLXN5c3RlbSwgQmxpbmtNYWNTeXN0ZW1Gb250LCAmcXVvdDtTZWdvZSBVSSZxdW90Oywgc3lzdGVtLXVpLCBzYW5zLXNlcmlmOyI+MC4wMDwvdGV4dD48dGV4dCB4PSIyMDMuNzEiIHk9IjI4NC4yMCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgc3R5bGU9ImZvbnQtc2l6ZTogMS4zNmVtO2ZpbGw6ICM0RDRENEQ7IGZvbnQtZmFtaWx5OiAmcXVvdDtSb2JvdG8mcXVvdDssIC1hcHBsZS1zeXN0ZW0sIEJsaW5rTWFjU3lzdGVtRm9udCwgJnF1b3Q7U2Vnb2UgVUkmcXVvdDssIHN5c3RlbS11aSwgc2Fucy1zZXJpZjsiPjAuNTA8L3RleHQ+PHRleHQgeD0iMjY3LjI2IiB5PSIzMDIuNTQiIHRleHQtYW5jaG9yPSJtaWRkbGUiIHN0eWxlPSJmb250LXNpemU6IDEuMzZlbTtmaWxsOiAjNEQ0RDREOyBmb250LWZhbWlseTogJnF1b3Q7Um9ib3RvJnF1b3Q7LCAtYXBwbGUtc3lzdGVtLCBCbGlua01hY1N5c3RlbUZvbnQsICZxdW90O1NlZ29lIFVJJnF1b3Q7LCBzeXN0ZW0tdWksIHNhbnMtc2VyaWY7Ij4xLjAwPC90ZXh0Pjx0ZXh0IHRyYW5zZm9ybT0idHJhbnNsYXRlKDE5NS45MCwzMDYuMzgpIHJvdGF0ZSgxNikiIHRleHQtYW5jaG9yPSJtaWRkbGUiIHN0eWxlPSJmb250LXNpemU6IDEuN2VtOyBmb250LWZhbWlseTogJnF1b3Q7Um9ib3RvJnF1b3Q7LCAtYXBwbGUtc3lzdGVtLCBCbGlua01hY1N5c3RlbUZvbnQsICZxdW90O1NlZ29lIFVJJnF1b3Q7LCBzeXN0ZW0tdWksIHNhbnMtc2VyaWY7Ij54PC90ZXh0Pjx0ZXh0IHg9IjMxMS4yOSIgeT0iMjg1LjQ2IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBzdHlsZT0iZm9udC1zaXplOiAxLjM2ZW07ZmlsbDogIzRENEQ0RDsgZm9udC1mYW1pbHk6ICZxdW90O1JvYm90byZxdW90OywgLWFwcGxlLXN5c3RlbSwgQmxpbmtNYWNTeXN0ZW1Gb250LCAmcXVvdDtTZWdvZSBVSSZxdW90Oywgc3lzdGVtLXVpLCBzYW5zLXNlcmlmOyI+MC4yNTwvdGV4dD48dGV4dCB4PSIzMjkuNjQiIHk9IjI2OS41OCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgc3R5bGU9ImZvbnQtc2l6ZTogMS4zNmVtO2ZpbGw6ICM0RDRENEQ7IGZvbnQtZmFtaWx5OiAmcXVvdDtSb2JvdG8mcXVvdDssIC1hcHBsZS1zeXN0ZW0sIEJsaW5rTWFjU3lzdGVtRm9udCwgJnF1b3Q7U2Vnb2UgVUkmcXVvdDssIHN5c3RlbS11aSwgc2Fucy1zZXJpZjsiPjAuNTA8L3RleHQ+PHRleHQgeD0iMzQ3Ljk4IiB5PSIyNTMuNjkiIHRleHQtYW5jaG9yPSJtaWRkbGUiIHN0eWxlPSJmb250LXNpemU6IDEuMzZlbTtmaWxsOiAjNEQ0RDREOyBmb250LWZhbWlseTogJnF1b3Q7Um9ib3RvJnF1b3Q7LCAtYXBwbGUtc3lzdGVtLCBCbGlua01hY1N5c3RlbUZvbnQsICZxdW90O1NlZ29lIFVJJnF1b3Q7LCBzeXN0ZW0tdWksIHNhbnMtc2VyaWY7Ij4wLjc1PC90ZXh0Pjx0ZXh0IHg9IjM2Ni4zMyIgeT0iMjM3LjgwIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBzdHlsZT0iZm9udC1zaXplOiAxLjM2ZW07ZmlsbDogIzRENEQ0RDsgZm9udC1mYW1pbHk6ICZxdW90O1JvYm90byZxdW90OywgLWFwcGxlLXN5c3RlbSwgQmxpbmtNYWNTeXN0ZW1Gb250LCAmcXVvdDtTZWdvZSBVSSZxdW90Oywgc3lzdGVtLXVpLCBzYW5zLXNlcmlmOyI+MS4wMDwvdGV4dD48dGV4dCB0cmFuc2Zvcm09InRyYW5zbGF0ZSgzNTAuNjksMjg5LjAyKSByb3RhdGUoLTQxKSIgdGV4dC1hbmNob3I9Im1pZGRsZSIgc3R5bGU9ImZvbnQtc2l6ZTogMS43ZW07IGZvbnQtZmFtaWx5OiAmcXVvdDtSb2JvdG8mcXVvdDssIC1hcHBsZS1zeXN0ZW0sIEJsaW5rTWFjU3lzdGVtRm9udCwgJnF1b3Q7U2Vnb2UgVUkmcXVvdDssIHN5c3RlbS11aSwgc2Fucy1zZXJpZjsiPnk8L3RleHQ+PHRleHQgeD0iMTE5Ljk5IiB5PSIyNDQuMjEiIHRleHQtYW5jaG9yPSJtaWRkbGUiIHN0eWxlPSJmb250LXNpemU6IDEuMzZlbTtmaWxsOiAjNEQ0RDREOyBmb250LWZhbWlseTogJnF1b3Q7Um9ib3RvJnF1b3Q7LCAtYXBwbGUtc3lzdGVtLCBCbGlua01hY1N5c3RlbUZvbnQsICZxdW90O1NlZ29lIFVJJnF1b3Q7LCBzeXN0ZW0tdWksIHNhbnMtc2VyaWY7Ij4wLjAwPC90ZXh0Pjx0ZXh0IHg9IjExOS45OSIgeT0iMjEyLjQzIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBzdHlsZT0iZm9udC1zaXplOiAxLjM2ZW07ZmlsbDogIzRENEQ0RDsgZm9udC1mYW1pbHk6ICZxdW90O1JvYm90byZxdW90OywgLWFwcGxlLXN5c3RlbSwgQmxpbmtNYWNTeXN0ZW1Gb250LCAmcXVvdDtTZWdvZSBVSSZxdW90Oywgc3lzdGVtLXVpLCBzYW5zLXNlcmlmOyI+MC4yNTwvdGV4dD48dGV4dCB4PSIxMTkuOTkiIHk9IjE4MC42NiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgc3R5bGU9ImZvbnQtc2l6ZTogMS4zNmVtO2ZpbGw6ICM0RDRENEQ7IGZvbnQtZmFtaWx5OiAmcXVvdDtSb2JvdG8mcXVvdDssIC1hcHBsZS1zeXN0ZW0sIEJsaW5rTWFjU3lzdGVtRm9udCwgJnF1b3Q7U2Vnb2UgVUkmcXVvdDssIHN5c3RlbS11aSwgc2Fucy1zZXJpZjsiPjAuNTA8L3RleHQ+PHRleHQgeD0iMTE5Ljk5IiB5PSIxNDguODgiIHRleHQtYW5jaG9yPSJtaWRkbGUiIHN0eWxlPSJmb250LXNpemU6IDEuMzZlbTtmaWxsOiAjNEQ0RDREOyBmb250LWZhbWlseTogJnF1b3Q7Um9ib3RvJnF1b3Q7LCAtYXBwbGUtc3lzdGVtLCBCbGlua01hY1N5c3RlbUZvbnQsICZxdW90O1NlZ29lIFVJJnF1b3Q7LCBzeXN0ZW0tdWksIHNhbnMtc2VyaWY7Ij4wLjc1PC90ZXh0Pjx0ZXh0IHg9IjExOS45OSIgeT0iMTE3LjExIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBzdHlsZT0iZm9udC1zaXplOiAxLjM2ZW07ZmlsbDogIzRENEQ0RDsgZm9udC1mYW1pbHk6ICZxdW90O1JvYm90byZxdW90OywgLWFwcGxlLXN5c3RlbSwgQmxpbmtNYWNTeXN0ZW1Gb250LCAmcXVvdDtTZWdvZSBVSSZxdW90Oywgc3lzdGVtLXVpLCBzYW5zLXNlcmlmOyI+MS4wMDwvdGV4dD48dGV4dCB0cmFuc2Zvcm09InRyYW5zbGF0ZSgxMDAuNjEsMTc1Ljc5KSByb3RhdGUoLTkwKSIgdGV4dC1hbmNob3I9Im1pZGRsZSIgc3R5bGU9ImZvbnQtc2l6ZTogMS43ZW07IGZvbnQtZmFtaWx5OiAmcXVvdDtSb2JvdG8mcXVvdDssIC1hcHBsZS1zeXN0ZW0sIEJsaW5rTWFjU3lzdGVtRm9udCwgJnF1b3Q7U2Vnb2UgVUkmcXVvdDssIHN5c3RlbS11aSwgc2Fucy1zZXJpZjsiPno8L3RleHQ+PC9nPjwvZz48L3N2Zz4=)

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
