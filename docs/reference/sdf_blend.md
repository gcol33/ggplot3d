# Combine and transform signed distance functions

Constructive solid geometry on `ggplot3d_sdf` shapes
([`sdf_sphere()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md)).
`a | b` is the union (`min(a, b)`), `a & b` the intersection
(`max(a, b)`), `a - b` removes b from a (`max(a, -b)`) and `-a` swaps
inside and outside. As in Quilez's catalogue the union is the exact
distance outside the shape only, and intersections and differences are
lower bounds of the distance.

## Usage

``` r
sdf_blend(a, b, k = 0.1, mode = c("union", "intersection", "difference"))

sdf_offset(sdf, distance)

sdf_shell(sdf, thickness)

sdf_transform(sdf, translate = c(0, 0, 0), rotate = c(0, 0, 0), scale = 1)

# S3 method for class 'ggplot3d_sdf'
Ops(e1, e2)
```

## Arguments

- a, b:

  Shapes.

- k:

  Blend thickness, in distance units.

- mode:

  `"union"`, `"intersection"` or `"difference"` (a without b).

- sdf:

  A shape.

- distance:

  Offset distance.

- thickness:

  Wall thickness.

- translate:

  Translation vector.

- rotate:

  Rotation angles in degrees about x, y and z, applied in that order, or
  a 3 x 3 rotation matrix.

- scale:

  Positive uniform scale factor.

- e1, e2:

  Shapes.

## Value

A `ggplot3d_sdf` function of `x`, `y` and `z`.

## Details

`sdf_blend()` replaces the minimum (or maximum) by the quadratic
polynomial smooth minimum, with `k` the largest thickness added where
the shapes meet, in distance units. `sdf_offset()` moves the surface
outwards by `distance` (inwards when negative); `sdf_shell()` keeps a
wall of the given thickness centred on the surface. `sdf_transform()`
scales the shape uniformly about the origin, rotates it about the x, y
and z axes in turn and then translates it; both keep the distance exact.

Every result records a Lipschitz constant of its value: one for exact
distances and their combinations, so the value never exceeds the
distance to the surface.

## References

Quilez, I. Smooth minimum. <https://iquilezles.org/articles/smin/>

Quilez, I. Interior SDFs.
<https://iquilezles.org/articles/interiordistance/>

## Examples

``` r
cut <- sdf_box(size = c(1.6, 1.6, 1.6), radius = 0.1) - sdf_sphere(radius = 1)
blob <- sdf_blend(sdf_sphere(c(-0.5, 0, 0), 0.6), sdf_sphere(c(0.5, 0, 0), 0.6), k = 0.2)
cut
ggplot2::ggplot() +
  geom_isofunction_3d(fun = sdf_shell(blob, 0.1) & sdf_plane(c(0, 1, 0)), n = 20) +
  ggplot2::xlim(-1.3, 1.3) + ggplot2::ylim(-1, 1) +
  scale_z_continuous(limits = c(-1, 1)) + coord_3d()
```
