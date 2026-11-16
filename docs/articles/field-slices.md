# Slicing fields and point densities

Inspect values inside a three-dimensional field by drawing sections
through it. This article builds horizontal and oblique slices, adds
contour lines and estimates density slices from point observations. It
assumes the mappings in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md);
[Preparing data for 3D
layers](https://gillescolling.com/ggplot3d/articles/data-preparation.md)
explains coordinate keys, and [Rendering volumes and traced
surfaces](https://gillescolling.com/ggplot3d/articles/ray-marching.md)
shows views through a whole volume.

1.  Supply a complete coordinate grid and its values.

2.  Choose slice positions or an oblique plane.

3.  For point observations, choose a density bandwidth and inspect its
    effect.

## Example

``` r

library(ggplot2)
library(ggplot3d)
```

### Supplying a scalar field

A scalar field assigns one value to each position. The table needs
continuous x, y and z columns plus a numeric value column. This example
samples a smooth concentration centred at the origin, with faster decay
along y. Coordinates and values use arbitrary units.

``` r

axis <- seq(-1, 1, length.out = 11)
field <- expand.grid(x = axis, y = axis, z = axis)
field$value <- with(field, exp(-(x^2 + 2 * y^2 + z^2)))
stopifnot(nrow(field) == 11L^3L,
  !anyDuplicated(field[c("x", "y", "z")]))
range(field$value)
#> [1] 0.01831564 1.00000000
```

Map the scalar to `value` while z remains the vertical position. A
horizontal slice at z = 0 shows the field’s central cross-section. Its
default fill represents the mean interpolated value on each triangle.
Slices are unlit by default, so their colours follow the fill scale
directly.

``` r

p <- ggplot(field, aes(x, y, z = z, value = value))
colours <- scale_fill_gradient(low = "#DCEAF3", high = "#2E6F9E",
  limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"),
  name = "Value")
horizontal <- p + geom_slice_3d(zslice = 0) + colours +
  coord_3d(aspect = "data")
horizontal
```

![A horizontal section of the concentration field, highest at the origin
and decreasing faster along
y.](field-slices_files/figure-html/horizontal-1.svg)

The stat returns triangle corners with a slice label and a face value.
Inspecting these columns shows which section was drawn and the range
represented by its fill. Several corners belong to each face, so a row
count measures geometry in this table.

``` r

section <- layer_data(horizontal)
unique(section$slice)
#> [1] z = 0
#> Levels: z = 0
range(section$value)
#> [1] 0.07447751 0.96130193
```

### Choosing planes and contours

Pass several positions to draw parallel slices. Keeping the same fill
limits lets their colours be compared directly. At z = -0.6 and z = 0.6
this symmetric field has matching values; the view reveals different
parts of the two planes through their overlap.

``` r

p + geom_slice_3d(zslice = c(-0.6, 0.6)) + colours +
  coord_3d(aspect = "data")
```

![Two parallel sections at z minus 0.6 and plus 0.6, using the same
value scale.](field-slices_files/figure-html/parallel-1.svg)

The x and y slice arguments choose perpendicular sections. With all
slice arguments omitted, the layer draws one central plane along each
axis. Contour slices use the same positions; explicit breaks select the
values traced by their lines.

``` r

p + geom_slice_3d(zslice = 0) +
  geom_slice_contour_3d(zslice = 0, breaks = c(0.3, 0.5, 0.7),
                        colour = "black", linewidth = 0.6) +
  colours + coord_3d(aspect = "data", elevation = 55)
```

![The central horizontal field section with contour lines at values 0.3,
0.5 and 0.7.](field-slices_files/figure-html/contours-1.svg)

An oblique plane uses a normal vector and a point on the plane. The
normal below gives the plane x - y + z = 0 through the origin. The slice
is sampled on a lattice and clipped to the field box; `n` sets the
sampling resolution on this plane.

``` r

plane <- sdf_plane(c(1, -1, 1), point = c(0, 0, 0))
p + stat_slice_3d(slice = plane, n = 14) +
  stat_slice_contour_3d(slice = plane, n = 14,
    breaks = c(0.3, 0.5, 0.7), colour = "black", linewidth = 0.6) +
  colours + coord_3d(aspect = "data")
```

![An oblique slice through the origin, coloured by field value and
crossed by three contour
levels.](field-slices_files/figure-html/oblique-1.svg)

Slice positions use transformed coordinate units. For example, a z slice
at 1 lies at an original height of 10 after a base-ten logarithmic
transformation. [Scaling heights and facet
ranges](https://gillescolling.com/ggplot3d/articles/z-scale.md) explains
those transformations. Axis-aligned slices use the input grid positions
along their other axes; increasing `n` refines oblique sampling.

## Preserving missing field values

Every coordinate combination must appear once. A missing measurement
belongs in the value column as `NA`. Interpolation uses the surrounding
grid corners, so missing values remove triangles that depend on them.
This copy masks a small central column of the field while preserving all
coordinate rows.

``` r

masked <- field
masked$value[abs(masked$x) < 0.21 & abs(masked$y) < 0.21] <- NA_real_
stopifnot(nrow(masked) == nrow(field))
ggplot(masked, aes(x, y, z = z, value = value)) +
  geom_slice_3d(zslice = 0) + colours +
  coord_3d(aspect = "data", elevation = 55)
```

![The central horizontal slice with an opening around its missing
central field values.](field-slices_files/figure-html/masked-1.svg)

A plane outside the grid has no field section to draw. Check the
coordinate ranges when a requested slice is empty. Use [Building
surfaces and
solids](https://gillescolling.com/ggplot3d/articles/surfaces-solids.html#scalar-field-level-surfaces)
to extract a boundary at a chosen field value.

## Estimating density from points

Point observations supply positions alone. The density-slice layer
estimates a Gaussian product-kernel density on a grid, then sections
that estimate. These simulated observations form two clouds separated
along x, each with 40 points. A fixed seed reproduces the sample.

``` r

set.seed(17)
cloud <- data.frame(x = c(rnorm(40, -0.65, 0.18), rnorm(40, 0.65, 0.18)),
                    y = rnorm(80, 0, 0.25), z = rnorm(80, 0, 0.25))
stopifnot(nrow(cloud) == 80L)
summary(cloud)
#>        x                   y                   z           
#>  Min.   :-1.290105   Min.   :-0.549776   Min.   :-0.61369  
#>  1st Qu.:-0.631816   1st Qu.:-0.128130   1st Qu.:-0.24221  
#>  Median :-0.144341   Median : 0.007778   Median :-0.07926  
#>  Mean   : 0.003794   Mean   : 0.004676   Mean   :-0.02747  
#>  3rd Qu.: 0.666813   3rd Qu.: 0.181828   3rd Qu.: 0.15131  
#>  Max.   : 1.089619   Max.   : 0.729018   Max.   : 0.59830
```

The bandwidth controls smoothing in the units of each transformed axis.
Choose three bandwidths when the axes need different smoothing. The
scalar bandwidth here applies equally to all three axes, and the
explicit limits leave room around the observations.

``` r

points <- ggplot(cloud, aes(x, y, z = z))
density_colours <- scale_fill_gradient(low = "#DCEAF3", high = "#2E6F9E",
  limits = c(0, 1.5), breaks = c(0, 0.75, 1.5), labels = c("0", "0.75", "1.5"),
  name = "Density")
density <- points + geom_density_slice_3d(zslice = 0, h = 0.25, n = 16) +
  geom_point_3d(size = 1, colour = "black") +
  density_colours +
  xlim(-1.5, 1.5) + ylim(-1, 1) + zlim(-1, 1) + coord_3d()
density
```

![A density slice at z zero with two concentrations beneath the
simulated observations.](field-slices_files/figure-html/density-1.svg)

The computed density is density per three-dimensional volume, evaluated
on the plane. It retains the smoothing contribution of observations away
from that plane. It is suitable for locating concentrations through a
cloud.

``` r

range(layer_data(density)$density)
#> [1] 0.001119723 0.762238628
```

Increasing the bandwidth spreads each observation’s contribution over a
larger region. The next plot doubles the bandwidth and keeps the same
coordinates and colour limits so the two estimates can be compared. Grid
resolution controls sampling of that estimate; it is a separate choice
from smoothing.

``` r

points + stat_density_slice_3d(zslice = 0, h = 0.5, n = 16) +
  geom_point_3d(size = 1, colour = "black") +
  density_colours +
  xlim(-1.5, 1.5) + ylim(-1, 1) + zlim(-1, 1) + coord_3d()
```

![The same point cloud with a more broadly smoothed density section on
the same colour
scale.](field-slices_files/figure-html/broad-density-1.svg)

For nested density boundaries around the observations, use the
probability-content examples in [Building surfaces and
solids](https://gillescolling.com/ggplot3d/articles/surfaces-solids.html#trivariate-density).
For selected planes through supplied measurements, use the scalar-field
slices above. [Combining 2D and 3D
graphics](https://gillescolling.com/ggplot3d/articles/combining-2d-3d.html#small-multiples-in-depth)
places separate two-dimensional plots on parallel planes.
