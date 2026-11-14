# Show one 3D scene from several views

`facet_views()` draws all data of a
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
plot once per view, one panel per camera, like the multi-view drawings
of engineering graphics and the shared views of matplotlib's 3D axes.
Every panel shows the same scene with the same x, y and z scales and the
same magnification, so a length measures the same on the page in every
orthographic view.

## Usage

``` r
facet_views(
  views = c("front", "top", "right", "isometric"),
  layout = c("wrap", "third_angle", "first_angle"),
  nrow = NULL,
  ncol = NULL,
  labeller = "label_value",
  strip.position = "top",
  lights = NULL,
  combine = c("cross", "pair")
)
```

## Arguments

- views:

  Views to draw, in panel order: a character vector of
  [`camera_3d()`](https://gillescolling.com/ggplot3d/reference/camera_3d.md)
  preset names, or a named list whose entries are preset names or lists
  of camera arguments such as `camera_3d("front")` or
  `list(azimuth = 30, elevation = 20)`. Camera arguments a view leaves
  out come from
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).
  Names label the panels; unnamed preset entries are labelled by the
  preset name. `NULL` uses the coordinate's camera.

- layout:

  Panel placement: `"wrap"` fills a grid row by row, `"third_angle"` and
  `"first_angle"` place the principal views by the projection method.

- nrow, ncol:

  Grid dimensions of the `"wrap"` layout.

- labeller, strip.position:

  Panel strip labeller and position, as in
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  Use `theme(strip.text = element_blank())` for a drawing without view
  names.

- lights:

  NULL inherits coordinate lighting. A named list of lights from
  [`lights_3d()`](https://gillescolling.com/ggplot3d/reference/lights_3d.md),
  [`light_3d()`](https://gillescolling.com/ggplot3d/reference/light_3d.md)
  or the individual light constructors supplies lighting conditions; a
  NULL list entry selects an unlit panel.

- combine:

  `"cross"` visits the lights in order for each camera; `"pair"` pairs
  the specifications, broadcasting a singleton. Shared guides use the
  first panel's camera and lighting as their reference. Engineering
  layouts accept one lighting condition.

## Value

A ggplot2 Facet.

## Details

With `layout = "third_angle"` or `"first_angle"` the panels follow the
projection methods of ISO 5456-2. The views are recognised by name:
`"front"`, `"top"`, `"bottom"`, `"left"`, `"right"` and `"rear"`. In
third-angle projection the top view sits above the front view and the
right view to its right; in first-angle projection the top view sits
below the front view and the right view to its left. The rear view sits
at the end of the row. Further views, such as an isometric one, fill the
free corners, first the upper right. Views in one row share their
vertical page positions and views in one column their horizontal ones,
so an edge seen in two adjacent views lines up across them.

The layers and their statistics are computed once; the panels differ
only in their camera and lighting. The plot's layer data therefore all
belong to the first panel.

## See also

[`camera_3d()`](https://gillescolling.com/ggplot3d/reference/camera_3d.md)
for the named views,
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
with `link` to turn several views together in the browser.

## Examples

``` r
block <- expand.grid(x = 1:4, y = 1:3, z = 1:3)
block <- block[block$z == 1 | (block$x == 1 & block$y < 3), ]
p <- ggplot2::ggplot(block, ggplot2::aes(x, y, z = z)) +
  geom_voxel_3d(fill = "grey80", colour = "grey20") +
  coord_3d()
p + facet_views()
p + facet_views(c("front", "top", "right", iso = "isometric"), layout = "third_angle")
p + facet_views(c("front", "top", "right", iso = "isometric"), layout = "first_angle")
p + facet_views(list(a = list(azimuth = -30), b = list(azimuth = 60, zoom = 0.8)))
p + facet_views(views = NULL, lights = list(
  left = lights_3d(light_directional(c(-1, 1, 2))),
  right = lights_3d(light_directional(c(1, 1, 2)))))
```
