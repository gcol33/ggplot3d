# Signed distance functions

Constructors of implicit shapes as signed distance functions: negative
inside, positive outside, zero on the surface. The result is a function
of `x`, `y` and `z` that can be passed as `fun` to
[`stat_isofunction_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isofunction_3d.md),
where it defaults to `levels = 0`. It also carries the expression tree
it was built from, so the extraction evaluates it on whole boxes in
interval arithmetic (`adaptive = TRUE`) and takes vertex normals from
its exact gradient. Shapes combine with the operators in
[`sdf_blend()`](https://gillescolling.com/ggplot3d/reference/sdf_blend.md).

## Usage

``` r
sdf_sphere(centre = c(0, 0, 0), radius = 1)

sdf_box(centre = c(0, 0, 0), size = c(1, 1, 1), radius = 0)

sdf_torus(centre = c(0, 0, 0), major = 1, minor = 0.25)

sdf_capsule(a = c(0, 0, -0.5), b = c(0, 0, 0.5), radius = 0.25)

sdf_plane(normal = c(0, 0, 1), point = c(0, 0, 0))

sdf_gyroid(period = 2 * pi, centre = c(0, 0, 0))

sdf_metaballs(centres, radius = 1)
```

## Arguments

- centre:

  Centre of the shape.

- radius:

  Sphere radius, capsule radius, rounding radius of the box edges, or
  for `sdf_metaballs()` one radius per centre.

- size:

  Edge lengths of the box along x, y and z.

- major, minor:

  Torus radii: distance from the centre to the tube axis, and tube
  radius. The torus lies in the x-y plane.

- a, b:

  End points of the capsule axis.

- normal:

  Plane normal, pointing to the positive side.

- point:

  A point on the plane.

- period:

  Period of the gyroid along each axis.

- centres:

  Ball centres as a three-column matrix or data frame.

## Value

A `ggplot3d_sdf` function of `x`, `y` and `z`.

## Details

The formulas are those of Quilez's catalogue. `sdf_sphere()`,
`sdf_box()`, `sdf_torus()`, `sdf_capsule()` and `sdf_plane()` return the
exact Euclidean distance. `sdf_gyroid()` returns the gyroid function
divided by an upper bound of its gradient, a lower bound of the
distance. `sdf_metaballs()` returns one half minus a sum of Wyvill
kernels, which is no distance; its surface encloses a ball of each given
radius where the balls are apart and blends them where they meet.

## References

Quilez, I. Distance functions.
<https://iquilezles.org/articles/distfunctions/>

Wyvill, McPheeters and Wyvill (1986). Data structure for soft objects.
The Visual Computer 2, 227-234.
[doi:10.1007/BF01900346](https://doi.org/10.1007/BF01900346)

## See also

[`sdf_blend()`](https://gillescolling.com/ggplot3d/reference/sdf_blend.md)
for unions, intersections, differences, smooth blends, offsets, shells
and transforms.

## Examples

``` r
s <- sdf_sphere(radius = 1)
s(c(0, 2), 0, 0)
ggplot2::ggplot() +
  geom_isofunction_3d(fun = sdf_box(size = c(2, 1, 1), radius = 0.2), n = 16) +
  ggplot2::xlim(-1.5, 1.5) + ggplot2::ylim(-1.5, 1.5) +
  scale_z_continuous(limits = c(-1.5, 1.5)) + coord_3d()
```
