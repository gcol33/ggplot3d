# Pair a face of a Thurston fundamental domain

Pair a face of a Thurston fundamental domain

## Usage

``` r
thurston_portal_3d(normal, offset, matrix, lift = 0, fiber = FALSE)
```

## Arguments

- normal:

  Four coefficients of the domain face in the geometry's embedded
  coordinates. Interior points satisfy `sum(normal * point) < offset`.

- offset:

  Face offset.

- matrix:

  A nonsingular 4 by 4 isometry matrix carrying this face to its paired
  face. The differential transports the ray's tangent on crossing.

- lift:

  Fibre displacement for product geometries, or an additional multiple
  of `4*pi` selecting a lift of an SL2R isometry.

- fiber:

  Use `normal[4] * w < offset` in the unwrapped fibre coordinate of SL2R
  or a product space. The first three normal entries must be zero.

## Value

A face descriptor for the `quotient` argument of
[`backend_3d()`](https://gillescolling.com/ggplot3d/reference/backend_3d.md).

## Details

E3, Nil and Sol embed as `(x,y,z,1)`. S3 and H3 embed on the unit sphere
and hyperboloid with the distinguished fourth coordinate. Product spaces
use the base sphere or hyperboloid in the first three coordinates and
the Euclidean factor in the fourth. SL2R uses the four matrix-basis
coordinates of Coulon et al.; its fibre lift is stored separately.
Quadric and metric preservation are checked at crossings. Faces must map
to the interior side of their partner. Supply every face of the domain.

## Examples

``` r
m <- diag(4); m[1, 4] <- -2
thurston_portal_3d(c(1, 0, 0, 0), 1, m)
```
