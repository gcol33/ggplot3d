# Explore a scene with orbit controls

Draw a panel in a WebGL htmlwidget, using its mapped scene and initial
camera. Drag to orbit, use the wheel or pinch to zoom, and select Camera
code to copy the current angles into a coord_3d() call. Arrow keys
orbit; + and - zoom.

## Usage

``` r
view_3d(plot, width = 640L, height = 640L, panel = 1L, elementId = NULL)
```

## Arguments

- plot:

  A ggplot with
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

- width, height:

  Widget dimensions in pixels, default 640 each.

- panel:

  Panel index in layout order, default 1. Facet strips, legends, plot
  titles and ordinary two-dimensional annotation grobs stay in the
  static plot. Each widget displays one panel.

- elementId:

  Optional HTML element identifier, default NULL. In Shiny, camera
  changes set the input named paste0(elementId, "\_camera").

## Value

An htmlwidget of class ggplot3d_view. Its x contains schema_version,
scene (schema-1 indexed model geometry and mapped materials), furniture,
axes, camera, framing, viewport, reference, background and capabilities.
Geometry is transferred once; camera interaction stays in the browser.

## Details

Requires htmlwidgets. Opaque geometry uses a WebGL depth buffer.
Transparent primitives are sorted back to front by centre depth;
crossing transparent surfaces can differ from the exact static vector
backend. Text uses plain browser text, with whole-anchor visibility for
scene labels. Lighting uses the same linear-light colour calculation as
static output. Furniture and its labels rotate with the camera. No
external scripts load.

## Examples

``` r
if (requireNamespace("htmlwidgets", quietly = TRUE)) {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
    geom_point_3d(size = 3) + coord_3d()
  view_3d(p)
}
```
