# Position guides for three-dimensional axes

Tick labels and titles are measured in the drawing viewport. Automatic
placement uses an exterior cube edge. Integer positions 1 through 4
select a fixed edge in lexicographic order of its two fixed coordinates.
Labels are thinned automatically using their measured extents along the
edge.

## Usage

``` r
guide_axis_3d(
  title = ggplot2::waiver(),
  theme = NULL,
  position = "auto",
  check.overlap = FALSE,
  angle = ggplot2::waiver(),
  n.dodge = 1L,
  minor.ticks = FALSE,
  cap = "none",
  order = 0L
)
```

## Arguments

- title:

  Guide title, with waiver inheriting the scale or plot title.

- theme:

  Local theme overrides.

- position:

  "auto", a side (top, bottom, left, right), or edge 1 to 4.

- check.overlap:

  Use extra clearance between measured label rectangles.

- angle:

  Label rotation in degrees. Waiver keeps labels horizontal.

- n.dodge:

  Number of parallel label rows.

- minor.ticks:

  Draw minor ticks.

- cap:

  Trim the lower, upper, both, or neither end to major breaks.

- order:

  Guide ordering value.

## Value

A ggplot2 guide for a three-dimensional position axis.

## Examples

``` r
d <- data.frame(x = 1:3, y = c(1, 2, 1), z = 1:3)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_point_3d() + scale_z_continuous(guide = guide_axis_3d()) + coord_3d()
```
