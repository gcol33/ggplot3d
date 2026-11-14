# Ray-marched scalar volumes

Renders a complete rectilinear grid per raster sample. Coordinates are
interpreted in transformed scale units. Samples are trilinearly
interpolated. Absorption uses front-to-back emission-absorption
integration in linear light and stops at the nearest opaque mesh or
traced surface. Translucent raster geometry inside the volume composites
at its depth: the integral is split at each fragment. Overlapping
volumes combine their extinction and emission at each sample. Vector and
hybrid panels place the volume as an image; a vector primitive is drawn
under it where it lies behind the depth at which the image reaches half
its opacity.

## Usage

``` r
geom_volume_3d(
  mapping = NULL,
  data = NULL,
  ...,
  mode = c("absorption", "mip", "iso"),
  opacity = c(0, 1),
  colours = c("#132B43", "#56B1F7"),
  samples = 256L,
  level = NULL,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, ..., show.legend, inherit.aes:

  Standard layer arguments. Map `x`, `y`, `z` and `value` to the grid
  coordinates and scalar field.

- mode:

  `"absorption"`, `"mip"` or `"iso"`.

- opacity:

  Nonnegative extinction coefficients per transformed data unit, as an
  evenly spaced numeric ramp over the scalar range, or a function of
  scalar values. A constant coefficient s over distance d transmits
  exp(-s\*d).

- colours:

  Colour ramp, or a function mapping scalar values to colours.

- samples:

  Number of integration steps across the volume diagonal. Grid-cell
  boundaries also split integration intervals.

- level:

  Isovalue for `mode = "iso"`; defaults to the scalar-range midpoint.

## Value

A ggplot2 layer.

## Details

MIP finds the maximum of the interpolated field along each ray.
Isosurfaces find the first root of the cubic field along each crossed
grid cell, including roots between equal-sign cell endpoints, and use
its analytic gradient. These modes are independent of the absorption
sampling count.

## References

Max (1995). Optical models for direct volume rendering. IEEE
Transactions on Visualization and Computer Graphics 1, 99-108.
[doi:10.1109/2945.468400](https://doi.org/10.1109/2945.468400)

## Examples

``` r
a <- seq(-1, 1, length.out = 8)
d <- expand.grid(x = a, y = a, z = a)
d$v <- with(d, exp(-4 * (x^2 + y^2 + z^2)))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = v)) +
  geom_volume_3d(opacity = c(0, 3), samples = 32) +
  coord_3d(backend = backend_3d("raster", dpi = 60))
```
