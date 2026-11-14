# Small multiples on parallel planes

Splits the data by one or more variables, like
[`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html),
and draws the subsets on parallel planes stacked along one axis of a
single 3D panel, one translucent plane per subset, as matplotlib stacks
2D polygons and bars at several depths of one 3D axes. Statistics and
position adjustments run within each slice, so a histogram or a smooth
is computed for every subset; layers whose data lack the slicing
variables are repeated in every slice.

## Usage

``` r
facet_slices_3d(
  facets,
  along = "z",
  planes = TRUE,
  labels = TRUE,
  fill = "grey85",
  colour = "grey60",
  alpha = 0.35
)
```

## Arguments

- facets:

  Slicing variables:
  [`ggplot2::vars()`](https://ggplot2.tidyverse.org/reference/vars.html),
  a one-sided formula such as `~ year`, or a character vector of column
  names.

- along:

  Axis the slices are stacked along: `"z"`, `"y"` or `"x"`.

- planes:

  Whether to draw the translucent planes.

- labels:

  Whether to label each plane with its slice.

- fill, colour, alpha:

  Fill, outline colour and opacity of the planes.

## Value

An object to add to a plot with `+`: the facet, the plane layer (drawn
first) and the label layer.

## Details

The slice's position is set on the `along` aesthetic of every layer, so
the layers draw with the other two: with `along = "z"` ordinary x/y
layers lie on horizontal planes, and `coord_3d(up = "y")` turns those
planes upright, one behind the other. A single numeric slicing variable
places the slices at its values on a continuous scale; otherwise the
slices sit at the levels of a discrete scale, in the order of factor
levels or sorted values. Each plane carries its label, as a facet strip
does.

## See also

[`annotation_plot_3d()`](https://gillescolling.com/ggplot3d/reference/annotation_plot_3d.md)
for one whole 2D plot on a plane.

## Examples

``` r
d <- data.frame(year = rep(c(2000, 2010, 2020), each = 150),
  x = rnorm(450, mean = rep(c(0, 0.5, 1), each = 150)))
ggplot2::ggplot(d, ggplot2::aes(x)) +
  ggplot2::geom_density(fill = "steelblue", alpha = 0.6) +
  facet_slices_3d(~year) +
  coord_3d(up = "y", azimuth = 30, elevation = 20)
ggplot2::ggplot(d, ggplot2::aes(x)) +
  ggplot2::geom_histogram(bins = 15, position = "identity", fill = "grey60") +
  facet_slices_3d(~year, along = "z") +
  coord_3d()
```
