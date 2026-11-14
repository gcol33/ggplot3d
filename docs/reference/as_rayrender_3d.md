# Convert a 3D panel to a rayrender scene

Build a [rayrender](https://www.rayrender.net) scene from the triangles
of one panel, one object per layer with its vertex colours and a diffuse
material, and optionally path-trace it from the plot's camera. Paths
become tubes and points become solids, as in
[`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md).

## Usage

``` r
as_rayrender_3d(
  plot,
  panel = 1L,
  furniture = FALSE,
  size = 1,
  width = 640L,
  height = 640L,
  dpi = 96,
  render = FALSE,
  ...
)
```

## Arguments

- plot:

  A ggplot with
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

- panel, furniture, size, width, height, dpi:

  See
  [`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md).
  `width` and `height` also set the rendered image size.

- render:

  Whether to render the scene with
  [`rayrender::render_scene()`](http://www.rayrender.net/reference/render_scene.md)
  from the plot's camera, default FALSE.

- ...:

  Further arguments to
  [`rayrender::render_scene()`](http://www.rayrender.net/reference/render_scene.md)
  when `render` is TRUE, overriding the camera and the white background.

## Value

The rayrender scene, or with `render = TRUE` the value of
[`rayrender::render_scene()`](http://www.rayrender.net/reference/render_scene.md).

## Details

Coordinates are model coordinates with the vertical axis of the plot as
+z. Translucency is not transferred.

## Examples

``` r
if (requireNamespace("rayrender", quietly = TRUE)) {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
    geom_point_3d(ggplot2::aes(colour = cyl)) + coord_3d()
  scene <- as_rayrender_3d(p)
  nrow(scene)
}
```
