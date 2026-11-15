# Curvilinear and compositional frames

[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
places x, y and z along the three edges of a box. Some data have another
natural frame: a month is an angle on a circle, a direction is a point
on a sphere, a location is a longitude and latitude on a globe, and a
mixture of three or four parts sums to a whole. This article draws data
in each of those frames. Installation is covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md), and
[Camera and
framing](https://gillescolling.com/ggplot3d/articles/camera.md) covers
the camera arguments that every frame shares.

``` r

library(ggplot2)
library(ggplot3d)
```

## Cylindrical coordinates

[`coord_cylindrical_3d()`](https://gillescolling.com/ggplot3d/reference/coord_cylindrical_3d.md)
reads x as an angle around a vertical axis, y as the distance from that
axis and z as height. Twelve months then close into a ring, and December
sits next to January. The y limits start at 0 so that site 1 lies on a
ring of its own instead of on the axis.

``` r

months <- data.frame(month = rep(1:12, 2), site = rep(c(1, 2), each = 12),
                     value = c(3:8, 8:3, 2:7, 7:2))
ggplot(months, aes(month, site, z = value, colour = factor(site))) +
  geom_path_3d(linewidth = 0.8) + geom_point_3d(size = 2) +
  scale_y_continuous(limits = c(0, 2)) +
  scale_colour_manual(values = c("#2E6F9E", "#9E5E0F")) +
  coord_cylindrical_3d(elevation = 35)
```

![Monthly values for two sites on two concentric rings, rising toward
the middle of the year.](frames_files/figure-html/cylindrical-1.svg)

## Spherical coordinates

[`coord_spherical_3d()`](https://gillescolling.com/ggplot3d/reference/coord_spherical_3d.md)
reads x as longitude, y as latitude and z as the distance from the
centre. A radius that depends on latitude flattens or stretches the
sphere; here it bulges at the equator.

``` r

g <- expand.grid(lon = seq(0, 330, by = 30), lat = seq(-60, 60, by = 30))
g$r <- 1 + 0.2 * cos(g$lat * pi / 90)
ggplot(g, aes(lon, lat, z = r)) +
  geom_point_3d(colour = "#2E6F9E", size = 2) +
  coord_spherical_3d(elevation = 20)
```

![Points on a sphere that bulges at the equator, sampled every thirty
degrees of longitude and
latitude.](frames_files/figure-html/spherical-1.svg)

## A globe

[`coord_globe_3d()`](https://gillescolling.com/ggplot3d/reference/coord_globe_3d.md)
draws longitude, latitude and height on a globe seen from the point
given by `centre`. A path between two cities follows the great circle in
the projection. The height column lifts the route slightly above the
surface.

``` r

route <- data.frame(lon = c(-74, 140), lat = c(41, 36), height = 0.02)
ggplot(route, aes(lon, lat, z = height)) +
  geom_path_3d(colour = "#B5342B", linewidth = 1) +
  coord_globe_3d(centre = c(-150, 35))
```

![A globe centred on the North Pacific with a red route from New York to
Tokyo.](frames_files/figure-html/globe-1.svg)

[Spatial data](https://gillescolling.com/ggplot3d/articles/spatial.md)
draws rasters and simple features on a flat map with a third dimension.

## Ternary prisms

A composition of three parts that sum to a whole has two degrees of
freedom, which a ternary diagram draws as a triangle.
[`coord_ternary_3d()`](https://gillescolling.com/ggplot3d/reference/coord_ternary_3d.md)
erects a prism on that triangle and uses z for a response. The parts are
mapped to `a`, `b` and `c`; they are rescaled to sum to one, so
percentages and proportions give the same plot.

``` r

soil <- data.frame(sand = c(60, 20, 30, 10), silt = c(30, 50, 20, 30),
                   clay = c(10, 30, 50, 60), yield = c(2, 5, 4, 3))
ggplot(soil, aes(a = sand, b = silt, c = clay, z = yield)) +
  geom_point_3d(size = 3, colour = "#3F7A3D") + coord_ternary_3d()
```

![Four soil samples in a ternary prism of sand, silt and clay, raised by
crop yield.](frames_files/figure-html/ternary-1.svg)

## Quaternary simplices

Four parts have three degrees of freedom and fill a tetrahedron.
[`coord_simplex_3d()`](https://gillescolling.com/ggplot3d/reference/coord_simplex_3d.md)
maps `a`, `b`, `c` and `d` to the four corners, again after rescaling
each row to sum to one. Exponential draws normalised this way are
uniform on the simplex.

``` r

set.seed(1)
w <- matrix(stats::rexp(80), ncol = 4)
mix <- data.frame(a = w[, 1], b = w[, 2], c = w[, 3], d = w[, 4])
ggplot(mix, aes(a = a, b = b, c = c, d = d)) +
  geom_point_3d(colour = "#7A4F9E", size = 2) + coord_simplex_3d()
```

![Twenty compositions of four parts scattered inside a
tetrahedron.](frames_files/figure-html/simplex-1.svg)

## Where next

- [Camera and
  framing](https://gillescolling.com/ggplot3d/articles/camera.md): view
  angles, projections and stable bounds.

- [Curved spaces and
  spacetime](https://gillescolling.com/ggplot3d/articles/curved-spaces.md):
  spherical, hyperbolic and Minkowski geometry rather than curvilinear
  coordinates on flat space.
