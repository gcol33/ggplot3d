# Tubes and ribbons along curves

Sweeps a circle (tubes) or a line segment (ribbons) along each group's
path in the order of the rows, in transformed position units. The cross
section lies in the plane of a rotation-minimising frame, computed by
the double reflection method of Wang et al. (2008), so it does not spin
about the curve the way a Frenet frame does, and does not flip at
inflections. On a closed curve (`closed = TRUE`) the frame carried once
around the curve would miss its starting orientation by an angle; that
angle is spread over the curve in proportion to arc length, so the
surface closes without a seam.

## Usage

``` r
geom_tube_3d(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  sides = 16L,
  radius = NULL,
  radius.unit = "data",
  cap = c("round", "flat", "none"),
  closed = FALSE,
  spline = NULL,
  n = 100L,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_ribbon_3d(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  width = NULL,
  width.unit = "data",
  normal = c(0, 0, 1),
  closed = FALSE,
  spline = NULL,
  n = 100L,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- sides:

  Number of points on the tube's circle.

- radius, width:

  Cross-section size where the aesthetic is not mapped.

- radius.unit:

  Unit of the cross-section size: `"data"` (transformed position units)
  or `"mm"` (millimetres on the page).

- cap:

  End caps of a tube on an open curve: `"round"`, `"flat"` or `"none"`.

- closed:

  Whether the path is a closed loop.

- spline:

  NULL, or a
  [`stat_spline_3d()`](https://gillescolling.com/ggplot3d/reference/stat_spline_3d.md)
  type to smooth the path first.

- n:

  Number of spline samples.

- width.unit:

  Unit of ribbon width, with the same choices as `radius.unit`.

- normal:

  Direction the first cross section is oriented towards: the ribbon's
  surface normal, or the profile's first axis in stat_sweep_3d().

## Value

A ggplot2 layer.

## Details

Opaque filled tubes without an edge colour are analytic quadrics: a
cylinder, or a cone where the radius changes, along each segment of the
path and a sphere at each joint, so the tube is the sweep of a ball
along the path, drawn with exact outlines and per-sample depth (Sigg et
al. 2006). A segment takes the colour of its first point. Translucent
tubes, and tubes with edges, are swept meshes with `sides` points on
each cross section.

The `radius` (tubes) or `width` (ribbons) aesthetic varies the cross
section per point, and `twist` turns it about the curve by that many
degrees. Without either, the size is the `radius` or `width` parameter,
by default 2% (tubes) or 8% (ribbons) of the diagonal of the position
ranges. Tube ends are closed with hemispheres (`cap = "round"`) or disks
(`"flat"`), or left open (`"none"`). A ribbon starts with its surface
normal as close to `normal` as the curve allows.

Numeric aesthetics such as a continuous fill are taken at every point;
on meshes they are averaged over each face, so they change gradually
from ring to ring. `spline` passes the points through
[`stat_spline_3d()`](https://gillescolling.com/ggplot3d/reference/stat_spline_3d.md)
first, with `n` samples. Normals are angle-weighted and split where
faces meet at more than 60 degrees. Computed variables: `arc_length`
from the first point and `z_mean`, the height of the curve point for
tubes and the mean height of a face for other sweeps.

With `radius.unit = "mm"` (`width.unit` for ribbons) the size is
measured on the page, like a line width: the cross section is swept when
the plot is drawn, in the box the camera sees, so a tube is round on
screen whatever the box proportions and has its radius in millimetres at
the centre of the view, shrinking with distance under a perspective
camera, as millimetre spheres of
[`geom_sphere_3d()`](https://gillescolling.com/ggplot3d/reference/geom_sphere_3d.md)
do. Sizes then default to 1 mm (tube radius) and 4 mm (ribbon width),
and `z_mean` is the height of the curve point. Millimetre sizes need the
Euclidean coordinates of
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

## References

Wang, Juettler, Zheng and Liu (2008). Computation of rotation minimizing
frames. ACM Transactions on Graphics 27, 2.
[doi:10.1145/1330511.1330513](https://doi.org/10.1145/1330511.1330513)

Sigg, Weyrich, Botsch and Gross (2006). GPU-based ray-casting of
quadratic surfaces. Symposium on Point-Based Graphics.
[doi:10.2312/SPBG/SPBG06/059-065](https://doi.org/10.2312/SPBG/SPBG06/059-065)

## Examples

``` r
t <- seq(0, 4 * pi, length.out = 120)
helix <- data.frame(x = cos(t), y = sin(t), z = t / 4, t = t)
ggplot2::ggplot(helix, ggplot2::aes(x, y, z = z, fill = t)) +
  geom_tube_3d(radius = 0.12) + coord_3d()
knot <- data.frame(u = seq(0, 2 * pi, length.out = 13)[-13])
knot <- transform(knot, x = sin(u) + 2 * sin(2 * u), y = cos(u) - 2 * cos(2 * u), z = -sin(3 * u))
ggplot2::ggplot(knot, ggplot2::aes(x, y, z = z)) +
  geom_tube_3d(radius = 0.3, closed = TRUE, spline = "catmull-rom", n = 200) + coord_3d()
ggplot2::ggplot(helix, ggplot2::aes(x, y, z = z, twist = t * 20)) +
  geom_ribbon_3d(width = 0.4) + coord_3d()
ggplot2::ggplot(helix, ggplot2::aes(x, y, z = z, fill = t)) +
  geom_tube_3d(radius = 1.5, radius.unit = "mm") + coord_3d(aspect = c(3, 1, 1))
```
