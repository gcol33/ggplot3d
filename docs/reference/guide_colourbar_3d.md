# Colour guides that show the scene's shading

`guide_colourbar_3d()` with `along` an axis draws a colourbar beside the
selected box edge of that position guide. The axis supplies its ticks,
labels and title. The colour aesthetic and that position must map the
same expression in every layer using the colour scale. Scale
transformations may differ: colours are evaluated at the position axis's
original data values. With `along = NULL` it is an ordinary legend-box
colourbar
([`ggplot2::guide_colourbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html)).

## Usage

``` r
guide_colourbar_3d(
  along = "z",
  width = grid::unit(2, "mm"),
  gap = grid::unit(0.5, "mm"),
  nbin = 128L,
  shading.range = FALSE,
  ...
)

guide_coloursteps_3d(..., shading.range = TRUE)

guide_legend_3d(..., lit = TRUE, normal = c(0, 0, 1), shading.range = FALSE)

guide_bins_3d(..., lit = TRUE, normal = c(0, 0, 1), shading.range = FALSE)
```

## Arguments

- along:

  Position axis beside which the bar is drawn, one of `"x"`, `"y"`,
  `"z"`, or NULL for a legend-box colourbar.

- width, gap:

  Physical grid units for the axis colourbar width and its gap from the
  edge. Tick lengths include this space.

- nbin:

  Number of colour intervals along the bar.

- shading.range:

  `FALSE`, `TRUE`, or two angles in degrees in \[0, 180\], the span of
  light shown.

- ...:

  Arguments passed to
  [`ggplot2::guide_colourbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html),
  [`ggplot2::guide_coloursteps()`](https://ggplot2.tidyverse.org/reference/guide_coloursteps.html),
  [`ggplot2::guide_bins()`](https://ggplot2.tidyverse.org/reference/guide_bins.html)
  or
  [`ggplot2::guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html).

- lit:

  Whether shaded layers use lit facet keys.

- normal:

  Finite nonzero model-space facet normal, length three.

## Value

A ggplot2 guide.

## Details

`guide_coloursteps_3d()` and `guide_bins_3d()` are
[`ggplot2::guide_coloursteps()`](https://ggplot2.tidyverse.org/reference/guide_coloursteps.html)
and
[`ggplot2::guide_bins()`](https://ggplot2.tidyverse.org/reference/guide_bins.html)
with shading; `guide_legend_3d()` and `guide_bins_3d()` draw keys of
shaded layers as a small facet lit by the plot's light and first camera.
`normal` specifies the facet's model-space normal. Unshaded layers
retain their own key geometry.

`shading.range` shows what the light does to each colour. The colourbars
and colour steps become a two-dimensional swatch, value along the bar
and light across it; legend and bin keys get a strip below the facet.
The colours come from the same shading kernel as the faces, for a
surface whose normal makes each angle along the range with every light,
without the specular term, using the material of the first layer with
one. `TRUE` spans 90 to 0 degrees for the `"multiply"` and `"tint"`
modes of
[`lights_3d()`](https://gillescolling.com/ggplot3d/reference/lights_3d.md)
(from ambient to full light) and 180 to 0 degrees for the signed modes;
two angles in degrees, `c(from, to)`, set the span, and reversing them
reverses the strip. With several facet panels, guides use the plot's
light.

## References

Karline Soetaert, plot3D, `colkey` colour keys.

## Examples

``` r
d <- expand.grid(x = 1:4, y = 1:4)
d$z <- with(d, x + y)
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z))
p + geom_surface_3d() +
  ggplot2::scale_fill_viridis_c(guide = guide_colourbar_3d()) + coord_3d()
p + geom_surface_3d() +
  ggplot2::scale_fill_distiller(palette = "Spectral",
    guide = guide_colourbar_3d(along = NULL, shading.range = TRUE)) +
  coord_3d(light = light_3d(mode = "lightness"))
p + geom_surface_3d() +
  ggplot2::scale_fill_viridis_b(guide = guide_coloursteps_3d(shading.range = TRUE)) + coord_3d()
p + geom_col_3d(ggplot2::aes(fill = factor(x))) +
  ggplot2::guides(fill = guide_legend_3d(shading.range = TRUE)) + coord_3d()
```
