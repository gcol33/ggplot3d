# Alpha shapes of point clouds

Computes the Delaunay tetrahedralisation of the points and draws the
boundary of their alpha complex (Edelsbrunner and Mucke 1994). A
tetrahedron belongs to the complex when its circumradius is below
`radius`; a triangle belongs to it when it is a face of such a
tetrahedron, or when its own circumradius is below `radius` and its
smallest circumsphere contains no other point. Triangles of the complex
that are not shared by two of its tetrahedra are drawn, so the shape is
the surface of the union of small tetrahedra plus any thin sheets.
`radius = Inf` gives the convex hull; a small radius leaves only
isolated pieces.

## Usage

``` r
stat_alpha_shape_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  radius = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_alpha_shape_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- radius:

  The alpha of the alpha shape, as a radius in position units. `NULL`
  uses the smallest radius at which the tetrahedra of the complex touch
  every point. (The argument is not called `alpha` because that name
  sets opacity.)

## Details

The tetrahedralisation is a native Bowyer-Watson insertion with exact
orientation and in-sphere predicates, so it is the Delaunay
tetrahedralisation of the exact positions (ties among cospherical points
are broken by insertion order). Positions are taken after scale
transformation, so `radius` is in transformed position units. Triangles
bounding a tetrahedron face away from it (computed `solid` is `TRUE`);
sheets have no inside. The computed `radius` holds the value used.

## References

Edelsbrunner and Mucke (1994). Three-dimensional alpha shapes. ACM
Transactions on Graphics 13, 43-72.
[doi:10.1145/174462.156635](https://doi.org/10.1145/174462.156635)

Shewchuk (1997). Adaptive precision floating-point arithmetic and fast
robust geometric predicates. Discrete & Computational Geometry 18,
305-363. [doi:10.1007/PL00009321](https://doi.org/10.1007/PL00009321)

## Examples

``` r
set.seed(1)
u <- runif(300, 0, 2 * pi); v <- runif(300, 0, 2 * pi)
d <- data.frame(x = (1 + 0.4 * cos(v)) * cos(u), y = (1 + 0.4 * cos(v)) * sin(u),
                z = 0.4 * sin(v))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_alpha_shape_3d(radius = 0.35, fill = "steelblue") + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  stat_alpha_shape_3d(fill = "steelblue") + coord_3d()
```
