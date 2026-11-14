# Clipping planes

A clipping plane cuts the layers of a 3D panel and keeps the half-space
its normal points into. Give a list of them to
`coord_3d(clip_planes = )` to cut every layer, or to the `clip_planes`
argument of a layer to replace the coordinate system's list for that
layer; `clip_planes = list()` leaves a layer uncut.

## Usage

``` r
clip_plane_3d(normal, point = NULL, cap = FALSE)
```

## Arguments

- normal:

  Three numbers, the normal of the plane along x, y and z in the units
  of the transformed position scales. The plane keeps the side the
  normal points into.

- point:

  A point on the plane in data units: three numbers, or a named vector
  or list giving the coordinates along which the normal is nonzero, such
  as `c(x = 5)` for the plane `x = 5`. `NULL` takes the centre of the
  panel box.

- cap:

  Whether to close cut solids with a face on the plane.

## Value

A clipping plane for
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
or a layer's `clip_planes`.

## Details

The plane is set in data units and passed through the position scales,
so it is a plane in the space the scales draw, as in Makie's
`clip_planes` and rgl's `clipplanes3d()`. Faces are cut exactly, lines
are cut where they cross the plane, and points and text are kept or
dropped by their anchor. Panes, grid lines and axes are not cut. With
`cap = TRUE`, the cross-section of each closed surface cut by the plane
is closed by a face on the plane, like MATLAB's `isocaps()` and VTK's
`vtkClipClosedSurface`. Each closed cut outline is filled, less the
outlines nested inside it, so a hollow solid keeps its hole; the cap
takes the style of the faces around it. Surfaces that are not closed
leave open outlines and get no cap. Planes apply in order, so a later
plane also cuts the caps of earlier ones.

## See also

[`geom_plane_3d()`](https://gillescolling.com/ggplot3d/reference/geom_plane_3d.md)
draws a plane.

## Examples

``` r
clip_plane_3d(normal = c(1, 0, 0), point = c(x = 0))
d <- expand.grid(x = seq(-2, 2, length.out = 30), y = seq(-2, 2, length.out = 30))
d$z <- exp(-(d$x^2 + d$y^2))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "steelblue") +
  coord_3d(clip_planes = list(clip_plane_3d(c(1, 1, 0), c(0, 0, 0))))
ggplot2::ggplot(data.frame(x = 0, y = 0, z = 0), ggplot2::aes(x, y, z = z)) +
  geom_sphere_3d(radius = 1, fill = "tomato") +
  coord_3d(xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1),
    clip_planes = list(clip_plane_3d(c(0, -1, 0), c(y = 0.3), cap = TRUE)))
```
