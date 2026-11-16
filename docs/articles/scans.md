# Reading 3D scans and point clouds

A 3D scan arrives as a file: a textured mesh from a phone or tablet
LiDAR app, a coloured point cloud from photogrammetry, or a LAS file
from an airborne survey. This article reads each kind, draws it, and
ends with a surface rebuilt from scanned points. Installation is covered
in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md); the
textured mesh needs [png](https://cran.r-project.org/package=png) (JPEG
textures need [jpeg](https://cran.r-project.org/package=jpeg)), and the
airborne example uses the sample data of
[lidR](https://cran.r-project.org/package=lidR).

1.  Read a textured mesh with
    [`read_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/read_mesh_3d.md).

2.  Read a point cloud with
    [`read_points_3d()`](https://gillescolling.com/ggplot3d/reference/read_points_3d.md).

3.  Rebuild a surface from the points with
    [`stat_implicit_fit_3d()`](https://gillescolling.com/ggplot3d/reference/stat_implicit_fit_3d.md).

``` r

library(ggplot2)
library(ggplot3d)
```

## Textured meshes

Scanning apps on phones and tablets with a LiDAR sensor (Polycam,
Scaniverse, 3d Scanner App on iPhone and iPad Pro models) export a mesh
as OBJ, PLY or glTF (`.glb`), with the camera images baked into one
texture. The example below writes such an export: a scanned stone as an
OBJ file whose `vt` lines give texture coordinates, an MTL file naming
the texture, and the texture as a PNG. Like most scanner exports, it
puts the vertical axis on y.

Code that writes the scan files

``` r

dir <- file.path(tempdir(), "stone")
dir.create(dir, showWarnings = FALSE)
grid <- expand.grid(u = seq(0, 2 * pi, length.out = 49), v = seq(0, pi, length.out = 25))
r <- with(grid, 1 + 0.08 * sin(5 * u) * sin(3 * v) + 0.05 * cos(7 * v))
xyz <- with(grid, cbind(r * cos(u) * sin(v), -r * cos(v), r * sin(u) * sin(v)))
id <- function(i, j) (j - 1) * 49 + i
q <- expand.grid(i = 1:48, j = 1:24)
faces <- with(q, cbind(id(i, j), id(i + 1, j), id(i + 1, j + 1), id(i, j + 1)))
triangles <- rbind(faces[q$j > 1, c(1, 2, 3)], faces[q$j < 24, c(1, 3, 4)])
face_lines <- sprintf("f %d/%d %d/%d %d/%d",
  triangles[, 1], triangles[, 1], triangles[, 2], triangles[, 2],
  triangles[, 3], triangles[, 3])
tex <- array(0, c(64, 128, 3))
shade <- outer(seq(0.35, 0.75, length.out = 64), 0.15 * sin(seq(0, 12 * pi, length.out = 128)), "+")
tex[, , 1] <- shade; tex[, , 2] <- 0.85 * shade; tex[, , 3] <- 0.6 * shade
png::writePNG(pmin(pmax(tex, 0), 1), file.path(dir, "stone.png"))
writeLines(c("newmtl stone", "Kd 1 1 1", "map_Kd stone.png"), file.path(dir, "stone.mtl"))
writeLines(c("mtllib stone.mtl",
  sprintf("v %.5f %.5f %.5f", xyz[, 1], xyz[, 2], xyz[, 3]),
  sprintf("vt %.5f %.5f", grid$u / (2 * pi), 1 - grid$v / pi),
  "usemtl stone",
  face_lines), file.path(dir, "stone.obj"))
```

[`read_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/read_mesh_3d.md)
returns the corner table: one row per face corner, with the positions,
the texture coordinates `.texture_u` and `.texture_v`, and the material
name from `usemtl`. The texture image travels with it as the attribute
`"texture"`.

``` r

stone <- read_mesh_3d(file.path(dir, "stone.obj"))
head(stone[c(".face", ".corner", "x", "y", "z", ".texture_u", ".texture_v", "material")], 4)
#>   .face .corner       x        y       z .texture_u .texture_v material
#> 1     1       1 0.13450 -1.02162 0.00000    0.00000    0.95833    stone
#> 2     1       2 0.13576 -1.04010 0.01787    0.02083    0.95833    stone
#> 3     1       3 0.26212 -0.98669 0.03451    0.02083    0.91667    stone
#> 4     2       1 0.13576 -1.04010 0.01787    0.02083    0.95833    stone
attr(stone, "texture")
#> <ggplot3d texture> 128 x 64 pixels, opaque, interpolated
#> extent: u 0 to 1, v 0 to 1
```

Given as the layer’s data, the mesh drapes its own texture.
`coord_3d(up = "y")` stands the stone upright, since the file’s vertical
axis is y.

``` r

ggplot() +
  geom_mesh_3d(data = stone, shading = "smooth") +
  coord_3d(up = "y")
```

![A rounded stone with a bumpy surface, textured with brown horizontal
bands from the scan image.](scans_files/figure-html/draw-scan-1.svg)

A glTF or GLB export reads the same way, its node transforms applied and
its embedded image taken as the texture. A file with several texture
images gets them packed into one atlas.

## Point clouds

A point cloud has no faces: each row is one measured point, often with a
colour from the camera and a normal estimated by the scanner.
[`read_points_3d()`](https://gillescolling.com/ggplot3d/reference/read_points_3d.md)
reads PLY, LAS, XYZ, PTS and glTF point clouds into a data frame. The
example writes a binary PLY of 4000 coloured points on the same stone,
with a little noise along the normal, as a scan of its surface would
have.

``` r

set.seed(4)
n <- 4000
u <- runif(n, 0, 2 * pi); v <- acos(runif(n, -1, 1))
r <- 1 + 0.08 * sin(5 * u) * sin(3 * v) + 0.05 * cos(7 * v) + rnorm(n, sd = 0.01)
cloud <- cbind(r * cos(u) * sin(v), -r * cos(v), r * sin(u) * sin(v))
brightness <- 0.8 + 0.4 * (cloud[, 2] + 1.2) / 2.4
rgb <- round(255 * outer(brightness, c(0.55, 0.45, 0.3)))
path <- tempfile(fileext = ".ply")
con <- file(path, "wb")
writeLines(c("ply", "format binary_little_endian 1.0", paste("element vertex", n),
  "property float x", "property float y", "property float z",
  "property uchar red", "property uchar green", "property uchar blue", "end_header"), con, sep = "\n")
bytes <- rbind(matrix(writeBin(as.vector(t(cloud)), raw(), size = 4, endian = "little"), 12),
               t(matrix(as.raw(pmin(rgb, 255)), n)))
writeBin(as.vector(bytes), con)
close(con)
```

The reader keeps every vertex property of the file as a column; `red`,
`green` and `blue` become one `colour` column of hex codes, which
[`I()`](https://rdrr.io/r/base/AsIs.html) passes to the plot unscaled.

``` r

points <- read_points_3d(path)
head(points, 3)
#>            x           y            z  colour
#> 1 -0.4678340  0.83316803 -0.279861003 #A08357
#> 2  0.1475692 -1.01664698  0.008303327 #745F40
#> 3 -0.2605559 -0.03929554  0.924092472 #8B724C
ggplot(points, aes(x, y, z = z, colour = I(colour))) +
  geom_point(size = 0.4) +
  coord_3d(up = "y")
```

![Four thousand brown points outlining the stone, lighter towards the
top.](scans_files/figure-html/read-points-1.svg)

Files from 3D Gaussian splatting tools are PLY point clouds too; their
parameters (`f_dc_0`, `opacity`, `scale_0`, `rot_0` and the rest) arrive
as columns by name.

## Drawing points as splats

[`geom_splat_3d()`](https://gillescolling.com/ggplot3d/reference/geom_splat_3d.md)
draws each point as a Gaussian. `size` gives its standard deviation in
the coordinate units of the scan. Overlapping footprints form a
continuous surface.

``` r

ggplot(points, aes(x, y, z = z, colour = I(colour))) +
  geom_splat_3d(size = 0.055) +
  coord_3d(up = "y")
```

![The scanned stone drawn with overlapping brown Gaussian
splats.](scans_files/figure-html/point-splats-1.svg)

A 3D Gaussian splatting capture stores logarithmic scales, quaternion
rotations, logit opacity and spherical-harmonic colour coefficients. The
layer activates these columns automatically. Here six anisotropic
Gaussians carry degree-zero colour coefficients, which give the same
colour from every direction.

``` r

capture <- data.frame(x = c(-0.6, 0, 0.6, -0.6, 0, 0.6),
  y = rep(c(-0.25, 0.25), each = 3), z = c(0, 0.1, 0, 0.1, 0, 0.1),
  scale_0 = log(0.28), scale_1 = log(0.13), scale_2 = log(0.06),
  opacity = qlogis(0.9), rot_0 = 2, rot_1 = 0, rot_2 = 0, rot_3 = 0)
colours <- t(grDevices::col2rgb(rep(c("steelblue", "seagreen", "coral"), 2))) / 255
for (k in 0:2) capture[[paste0("f_dc_", k)]] <- (colours[, k + 1] - 0.5) / 0.28209479177387814
ggplot(capture, aes(x, y, z = z)) +
  geom_splat_3d() +
  xlim(-1.5, 1.5) + ylim(-0.8, 0.8) + scale_z_continuous(limits = c(-0.25, 0.35)) +
  coord_3d(aspect = "data")
```

![Six overlapping anisotropic Gaussians in blue, green and
orange.](scans_files/figure-html/gaussian-capture-1.svg)

`sh_degree` limits the spherical-harmonic degree used for a capture’s
view-dependent colour. The default uses every available coefficient, up
to degree three. Explicit colour, fill, alpha, scale and quaternion
aesthetics override the corresponding capture properties.

## Rebuilding a surface

[`stat_implicit_fit_3d()`](https://gillescolling.com/ggplot3d/reference/stat_implicit_fit_3d.md)
with `method = "poisson"` fits screened Poisson surface reconstruction
to the points (Kazhdan and Hoppe
[2013](https://doi.org/10.1145/2487228.2487237)). The file holds no
normals, so the stat estimates each point’s normal from its 10 nearest
neighbours and orients them outwards. `depth` sets the finest octree
cell, here 1.1 / 2^6 of the stone’s width.

``` r

ggplot(points, aes(x, y, z = z)) +
  stat_implicit_fit_3d(method = "poisson", depth = 6, fill = "grey70", colour = NA) +
  coord_3d(up = "y")
```

![A closed smooth surface rebuilt from the points, the same bumpy stone
shape, in grey.](scans_files/figure-html/rebuild-1.svg)

## Airborne laser scanning

A LAS file stores a survey’s points with their return number,
classification and scan angle.
[`read_points_3d()`](https://gillescolling.com/ggplot3d/reference/read_points_3d.md)
reads uncompressed LAS 1.0 to 1.4 files natively; compressed LAZ files
are read by lidR
([`lidR::readLAS()`](https://rdrr.io/pkg/lidR/man/readLAS.html), then
[`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)).
The chunk below writes a thinned sample of lidR’s mixed conifer stand to
an uncompressed `.las` file and reads it back.

``` r

laz <- system.file("extdata", "MixedConifer.laz", package = "lidR")
las <- lidR::readLAS(laz, filter = "-keep_random_fraction 0.1")
```

``` r

file <- tempfile(fileext = ".las")
lidR::writeLAS(las, file)
stand <- read_points_3d(file)
nrow(stand)
#> [1] 3759
table(stand$return_number)
#> 
#>    1 
#> 3759
vapply(stand[c("x", "y", "z")], function(v) diff(range(v)), numeric(1))
#>     x     y     z 
#> 89.98 89.85 31.90
```

The coordinates are projected metres, and the last line gives the extent
of the stand along each axis. Colouring by height shows the crowns above
the ground returns.

``` r

ggplot(stand, aes(x, y, z = z, colour = z)) +
  geom_point(size = 0.2) +
  scale_colour_viridis_c(name = "Height (m)") +
  coord_3d(aspect = c(1, 1, 0.4))
```

![A forest stand as points, ground returns dark at the bottom and tree
crowns in lighter colours
above.](scans_files/figure-html/las-plot-1.svg)

## Where next

- [Reading and measuring
  meshes](https://gillescolling.com/ggplot3d/articles/meshes.md):
  decimate, smooth and measure a mesh read from a file.

- [Plotting maps and spatial
  data](https://gillescolling.com/ggplot3d/articles/spatial.md):
  terrain, simple features and lidR point clouds on maps.

- [Lighting and styling
  surfaces](https://gillescolling.com/ggplot3d/articles/lighting.md):
  materials and light for the scanned surface.

## References

Kazhdan, M. and Hoppe, H. (2013). Screened Poisson surface
reconstruction. *ACM Transactions on Graphics* 32, 29.
<https://doi.org/10.1145/2487228.2487237>
