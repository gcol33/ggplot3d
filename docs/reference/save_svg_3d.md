# Save an SVG with source rows and hover text

Writes compiled, depth-ordered fragments as SVG geometry. Each fragment
carries data-layer, data-row and data-primitive attributes. Consecutive
runs of each layer share a group so that interleaved layers retain their
visibility order. Scene masks become clipping paths, and surface seams
share compound-path underlays. Static plot furniture is embedded as SVG.

## Usage

``` r
save_svg_3d(plot, file, interactive = TRUE, width = 640L, height = 640L)
```

## Arguments

- plot:

  A ggplot with
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

- file:

  Output SVG path.

- interactive:

  Whether to include tooltips, keyboard focus and linked hover
  highlighting for fragments of the same source row.

- width, height:

  Plot dimensions in pixels at 96 pixels per inch.

## Value

`file`, invisibly.

## Examples

``` r
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp, tooltip = rownames(mtcars))) +
  geom_point_3d() + coord_3d()
if (requireNamespace("svglite", quietly = TRUE) && requireNamespace("xml2", quietly = TRUE)) {
  save_svg_3d(p, tempfile(fileext = ".svg"))
}
```
