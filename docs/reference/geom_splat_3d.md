# Gaussian splats in three dimensions

Draw point clouds with Gaussian footprints, or render Gaussian captures
read by
[`read_points_3d()`](https://gillescolling.com/ggplot3d/reference/read_points_3d.md).
Each row is one Gaussian.

## Usage

``` r
geom_splat_3d(
  mapping = NULL,
  data = NULL,
  ...,
  sh_degree = NULL,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, ..., show.legend, inherit.aes:

  As in
  [`geom_point_3d()`](https://gillescolling.com/ggplot3d/reference/geom_point_3d.md).

- sh_degree:

  Maximum spherical-harmonic degree, from 0 to 3. `NULL` uses all
  available coefficients.

## Details

`size` is an isotropic standard deviation in data units (default 0.02).
`scale_x`, `scale_y`, `scale_z` give anisotropic standard deviations;
`qw`, `qx`, `qy`, `qz` give a rotation quaternion, normalised
internally. Mapped `size` follows the usual size scale; use `I(size)` to
keep data units. The other covariance aesthetics are used directly.

Unmapped properties are read from INRIA-format 3D Gaussian splatting
columns: `scale_0` through `scale_2` are exponentiated, `opacity` is
logistic-transformed, and `rot_0` through `rot_3` are normalised.
`f_dc_0` through `f_dc_2` and optional channel-major `f_rest_*`
coefficients give camera-dependent RGB colour. Explicit colour or fill
overrides these coefficients. Ordinary colour and fill scales and
`colour = I(hex)` work as for points.

Raster, vector and hybrid panels use depth-composited EWA footprints
truncated at three standard deviations. Path tracing uses Gaussian
responses along rays with the colour as surface albedo. The viewer and
scene exports display coloured points. The stroke-only plotter omits
splats. Splats require an affine Cartesian frame and position scales.

## References

Kerbl et al. (2023). 3D Gaussian Splatting for Real-Time Radiance Field
Rendering. [doi:10.1145/3592433](https://doi.org/10.1145/3592433) .

Zwicker et al. (2002). EWA Splatting.
[doi:10.1109/TVCG.2002.1021576](https://doi.org/10.1109/TVCG.2002.1021576)
.

Moenne-Loccoz et al. (2024). 3D Gaussian Ray Tracing: Fast Tracing of
Particle Scenes. [doi:10.1145/3687934](https://doi.org/10.1145/3687934)
.

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 12),
                 y = seq(-1, 1, length.out = 12))
d$z <- with(d, 0.2 * cos(3 * x) * sin(3 * y))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, colour = z)) +
  geom_splat_3d(size = 0.12) + coord_3d()
```
