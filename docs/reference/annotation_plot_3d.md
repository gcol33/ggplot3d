# A two-dimensional plot on a plane of the panel box

Draws the panel of a whole two-dimensional ggplot, every layer with its
panel background and grid lines, on a plane of the 3D panel, such as a
wall behind the data. The plot's x runs along the first axis of `plane`
and its y along the second; the remaining axis is fixed at `at`, where
`Inf` and `-Inf` place the plot on the box sides at the upper and lower
end of that axis (0 when no other layer trains that axis). By default
the plot keeps its own coordinates: its panel covers its x and y ranges
on the two plane axes, so its data line up with the 3D layers that share
those axes. `extent` places it elsewhere.

## Usage

``` r
annotation_plot_3d(
  plot,
  plane = "xz",
  at = Inf,
  extent = NULL,
  route = c("vector", "raster"),
  resolution = 150,
  alpha = 1
)
```

## Arguments

- plot:

  A two-dimensional ggplot with a single panel.

- plane:

  Two distinct axis letters: the axis carrying the plot's x then the
  axis carrying its y, for example `"xz"`, `"yz"` or `"xy"`.

- at:

  Position on the remaining axis.

- extent:

  `NULL`, or four numbers `c(min, max, min, max)`: the positions of the
  plot's panel along the two plane axes. A reversed pair mirrors the
  plot.

- route:

  `"vector"` or `"raster"` (which requires ragg).

- resolution:

  Pixels along each side of the raster image.

- alpha:

  Opacity multiplying the plot's own.

## Value

A ggplot2 layer.

## Details

The vector route decomposes the drawn panel into points, lines,
polygons, rectangles and text that join the 3D scene and its exact
visibility, as matplotlib places 2D artists on a plane of 3D axes and
ggcube projects layers onto the faces of its cube. Marks lie in the
plane, foreshortened with it, at the size they have on a 2D panel 4
inches wide; text faces the viewer and is drawn over the scene. The
raster route renders the panel to an image of `resolution` pixels a
side, as rayshader does for a whole ggplot, and lays its pixels on the
plane as rectangles, joining runs of equal colour; it suits dense plots
whose vector form would be large.

## References

Hunter (2007). Matplotlib: a 2D graphics environment. Computing in
Science & Engineering 9, 90-95.
[doi:10.1109/MCSE.2007.55](https://doi.org/10.1109/MCSE.2007.55)

## See also

[`facet_slices_3d()`](https://gillescolling.com/ggplot3d/reference/facet_slices_3d.md)
for small multiples on parallel planes.

## Examples

``` r
d <- data.frame(x = rnorm(200), y = rnorm(200))
d$z <- d$x * d$y + rnorm(200, sd = 0.3)
side <- ggplot2::ggplot(d, ggplot2::aes(x, z)) +
  ggplot2::geom_point(colour = "grey40", size = 0.8) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  annotation_plot_3d(side, plane = "xz", at = Inf) +
  geom_point_3d(colour = "steelblue") +
  coord_3d()
```
