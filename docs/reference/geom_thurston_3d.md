# Draw an intrinsic ball, tube, or half-space

Draw an intrinsic ball, tube, or half-space

## Usage

``` r
geom_thurston_3d(
  shape = "ball",
  center = c(0, 0, 2),
  radius = 0.5,
  direction = c(0, 0, 1),
  ...
)
```

## Arguments

- shape:

  `"ball"`, `"cylinder"`, or `"halfspace"`.

- center:

  Three coordinates of the centre or a point on the boundary.

- radius:

  Positive intrinsic radius for balls and cylinders.

- direction:

  Unit normal for a half-space at its centre. Nil and Sol use chart
  half-spaces; product spaces support vertical or horizontal ones.

- ...:

  Material parameters including fill.

## Value

A three-dimensional ggplot layer.

## Details

This layer uses `backend_3d("raymarch")`. Balls use intrinsic distance
in E3, S3, H3, the product spaces, Nil and SL2R. Sol balls use the
explicitly approximate level set
`sqrt(exp(-2*z)*x^2 + exp(2*z)*y^2 + z^2) = radius` in coordinates
left-translated to the centre, as in Coulon et al. section 11.5.
Cylinders surround the vertical geodesic or fibre through the centre;
Sol cylinders surround the left translate of the x-axis subgroup. S3
cylinder radii must be below pi/2 and S3 ball radii below pi. General
chart-defined CSG surfaces use
[`geom_isofunction_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isofunction_3d.md)
with `render = "trace"`; their SDF algebra is compiled by the shared
registry.

## Examples

``` r
ggplot2::ggplot() + geom_thurston_3d(center = c(0, 0, 2), fill = "steelblue") +
  coord_3d(backend = backend_3d("raymarch", geometry = "nil", dpi = 40))
```
