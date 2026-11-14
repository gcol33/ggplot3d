# Hatching of surfaces and meshes

`hatch_3d()` draws a surface or mesh layer's tone as hatching, passed as
the layer's `hatch` argument. With `space = "model"` the strokes are the
intersections of the triangles with parallel planes: `angle` turns the
planes' normal in the panel's x-y plane from the x axis, so `angle = 0`
gives lines of constant x (the iso-parameter lines of a grid surface)
and `angle = 90` lines of constant y; `cross = TRUE` adds the
perpendicular family. The strokes are lines of the scene, hidden by
nearer surfaces like any other line, so in vector output they are exact;
with a white fill, or with `backend_3d("plotter")`, the figure is drawn
with lines only.

## Usage

``` r
hatch_3d(
  angle = 45,
  spacing = NULL,
  cross = TRUE,
  tone = TRUE,
  space = c("model", "screen"),
  colour = "black",
  linewidth = 0.25,
  alpha = 1
)
```

## Arguments

- angle:

  Angle in degrees of the first family.

- spacing:

  Distance between lines: a fraction of the panel box side for
  `space = "model"`, mm for `"screen"`. NULL takes 0.015 and 1.2.

- cross:

  Whether the dark tones add the perpendicular family.

- tone:

  Whether line density follows the shading.

- space:

  `"model"` or `"screen"`.

- colour, linewidth, alpha:

  Stroke of the lines.

## Value

A hatching specification for the `hatch` argument of
[`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md),
[`geom_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/geom_mesh_3d.md)
and the surface stats.

## Details

With `tone = TRUE` the density follows the shading: every face is lit
(as a white face of the layer's material) and its darkness, one minus
the encoded grey, selects how many lines it keeps, from every fourth
line in light tones to every line, then the cross family, in dark tones.
The lines kept at one tone contain those kept at every lighter tone, the
nesting of the tonal art maps of Praun et al. (2001), so strokes run on
unbroken where the tone changes. Without a light every line of the first
family is drawn.

With `space = "screen"` the faces get a hatch pattern in device space
instead (a [`grid::pattern()`](https://rdrr.io/r/grid/patterns.html)
fill per tone band, as in Murrell's vector pattern fills): lines
`spacing` mm apart, twice as far in light faces, crossed in dark ones.
It needs a device with pattern fills (ragg, svg, cairo_pdf, pdf) and is
drawn by the vector backend.

## References

Praun, Hoppe, Webb, Finkelstein (2001). Real-time hatching. SIGGRAPH
2001, 581-586.
[doi:10.1145/383259.383328](https://doi.org/10.1145/383259.383328)

Hertzmann and Zorin (2000). Illustrating smooth surfaces. SIGGRAPH 2000,
517-526.
[doi:10.1145/344779.345074](https://doi.org/10.1145/344779.345074)

## Examples

``` r
d <- expand.grid(x = seq(-2, 2, length.out = 30), y = seq(-2, 2, length.out = 30))
d$z <- with(d, exp(-(x^2 + y^2)))
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
p + geom_surface_3d(fill = "white", colour = NA, hatch = hatch_3d(angle = 0)) + coord_3d()
p + geom_surface_3d(fill = "white", colour = NA, hatch = hatch_3d(space = "screen")) + coord_3d()
```
