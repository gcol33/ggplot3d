# Register a geometry emitter

Register a geometry emitter

## Usage

``` r
register_geom_3d(
  geom,
  emit,
  aesthetics = character(),
  setup_data = NULL,
  stat_adapter = NULL,
  position_check = NULL,
  replace = FALSE,
  prepare_data = NULL,
  prepare_aesthetics = NULL
)
```

## Arguments

- geom:

  A ggplot2 geom prototype.

- emit:

  Function accepting data, params and panel_params.

- aesthetics:

  Additional supported aesthetics.

- setup_data:

  Optional geometry setup function.

- stat_adapter:

  Optional stat prototype.

- position_check:

  Optional position validation function.

- replace:

  Whether to replace an existing registration.

- prepare_data, prepare_aesthetics:

  Optional input and aesthetic preparation functions.

## Value

NULL invisibly. Registers the emitter for subsequent layers.

## Details

The emitter receives scale-transformed positions normalised to the panel
box. It returns a scene packet from
[`new_scene_packet()`](https://gillescolling.com/ggplot3d/reference/new_scene_packet.md).
Vertices hold x, y, z; corners index vertices; primitive offsets index
corners. Style IDs index the style table. Source rows and layer IDs
preserve input identity.

## Examples

``` r
GeomTriangle3D <- ggplot2::ggproto("GeomTriangle3D", ggplot2::Geom,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(fill = "steelblue", alpha = 1),
  draw_key = ggplot2::draw_key_polygon)
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
register_geom_3d(GeomTriangle3D, emit_triangle, replace = TRUE)
triangle <- ggplot2::layer(geom = GeomTriangle3D, stat = "identity",
  position = "identity", params = list())
d <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 1, 1))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  layer_3d(triangle) + coord_3d()
```
