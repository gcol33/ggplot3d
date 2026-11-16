# Read a point cloud file

Reads LiDAR and photogrammetry point clouds into a data frame with one
row per point, ready for
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
with `aes(z = z)` in
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md),
or for surface reconstruction with
[`stat_implicit_fit_3d()`](https://gillescolling.com/ggplot3d/reference/stat_implicit_fit_3d.md)
and
[`stat_alpha_shape_3d()`](https://gillescolling.com/ggplot3d/reference/stat_alpha_shape_3d.md).
The reader is chosen by file extension from a registry of native
parsers.

## Usage

``` r
read_points_3d(path)
```

## Arguments

- path:

  Path to a `.ply`, `.las`, `.xyz`, `.pts`, `.txt`, `.gltf` or `.glb`
  file.

## Value

A data frame with columns `x`, `y`, `z` and the point attributes.

## Details

- PLY (ASCII, binary little- or big-endian): the `vertex` element;
  faces, if any, are ignored. `red`, `green`, `blue` (and `alpha`)
  become `colour`; every other scalar property is kept as a column, so
  normals (`nx`, `ny`, `nz`) and the parameters of 3D Gaussian splatting
  files (`f_dc_0`, `opacity`, `scale_0`, `rot_0`, ...) come through by
  name.

- LAS 1.0 to 1.4, point formats 0 to 10: coordinates with the header's
  scale and offset applied, `intensity`, `return_number`,
  `number_of_returns`, `classification`, `scan_angle_rank` (formats 0 to
  5, degrees) or `scan_angle` (formats 6 to 10, degrees), `user_data`,
  `point_source_id`, `gpstime` where the format records it, and `colour`
  and `nir` where it records them. Names follow lidR's columns in snake
  case. Compressed LAZ files are read by lidR
  ([`lidR::readLAS()`](https://rdrr.io/pkg/lidR/man/readLAS.html), then
  [`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)).

- XYZ and PTS text: whitespace, comma or semicolon separated columns,
  one point per line; a PTS file may start with the point count. Three
  columns are `x`, `y`, `z`; four add `intensity`; six add `colour` when
  the last three are integers in 0 to 255 and normals `nx`, `ny`, `nz`
  otherwise; seven add `intensity` and `colour`. Further columns are
  kept as `v4`, `v5`, and so on.

- glTF 2.0 (`.gltf`, `.glb`): the points of point primitives (`mode` 0),
  placed by their node transforms, with `COLOR_0` as `colour`.

Scanning apps on phones and tablets with a LiDAR sensor (iPhone and iPad
Pro models) export PLY, LAS, XYZ or glTF point clouds. Most of them
write y-up coordinates; draw those with `coord_3d(up = "y")`.

## References

LAS Specification 1.4 R15 (ASPRS, 2019):
<https://www.asprs.org/wp-content/uploads/2019/07/LAS_1_4_r15.pdf>. PLY
(Turk): <https://paulbourke.net/dataformats/ply/>. Kerbl, Kopanas,
Leimkuehler, Drettakis (2023). 3D Gaussian splatting for real-time
radiance field rendering. ACM Transactions on Graphics 42(4), 139.
[doi:10.1145/3592433](https://doi.org/10.1145/3592433)

## See also

[`read_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/read_mesh_3d.md)
for meshes.

## Examples

``` r
path <- tempfile(fileext = ".xyz")
set.seed(1)
u <- runif(400, 0, 2 * pi); v <- acos(runif(400, -1, 1))
writeLines(paste(cos(u) * sin(v), sin(u) * sin(v), cos(v)), path)
pts <- read_points_3d(path)
ggplot2::ggplot(pts, ggplot2::aes(x, y, z = z, colour = z)) +
  ggplot2::geom_point() +
  coord_3d()
```
