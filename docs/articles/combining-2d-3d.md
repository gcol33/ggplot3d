# Combining 2D and 3D graphics

A three-dimensional panel often needs two-dimensional companions:
ordinary ggplot2 layers drawn inside it, flat projections of the data on
the floor and walls of the box, or a contour map and a profile next to
it. This article shows each of these, then arranges 3D panels with 2D
panels using a second package. Installation is covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md). The
composition sections also use
[patchwork](https://patchwork.data-imaginist.com), the package that
arranges several ggplot2 plots on one page.

``` r

library(patchwork)
```

## Plain ggplot2 layers in a 3D panel

The data below trace a helix. `track` contains an angle `t`, the
positions `x` and `y` on a unit circle, and a height `z` that rises from
1 to 3 over two turns. The ends are labelled in a second table.

``` r

track <- data.frame(t = seq(0, 4 * pi, length.out = 80))
track$x <- cos(track$t)
track$y <- sin(track$t)
track$z <- 1 + track$t / (2 * pi)
ends <- track[c(1, nrow(track)), ]
ends$label <- c("start", "end")
```

[`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html),
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
and
[`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
are the ggplot2 layers, used unchanged. Inside
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
they read the z mapping and are placed and depth sorted like
[`geom_path_3d()`](https://gillescolling.com/ggplot3d/reference/geom_point_3d.md),
[`geom_point_3d()`](https://gillescolling.com/ggplot3d/reference/geom_point_3d.md)
and
[`geom_text_3d()`](https://gillescolling.com/ggplot3d/reference/geom_point_3d.md).

``` r

h <- ggplot(track, aes(x, y, z = z))
h + geom_path(colour = PAL["blue"], linewidth = 0.8) +
  geom_point(data = ends, colour = PAL["red"], size = 3) +
  geom_text(aes(label = label), data = ends, nudge_x = 0.35, size = 5) +
  coord_3d()
```

![A blue helix rising through two turns, with red points and the labels
start and end at its two
ends.](combining-2d-3d_files/figure-html/plain-layers-1.svg)

The accepted ggplot2 geoms are points, paths, lines, segments, text,
polygons and
[`geom_blank()`](https://ggplot2.tidyverse.org/reference/geom_blank.html);
[`geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html)
and
[`geom_density_2d()`](https://ggplot2.tidyverse.org/reference/geom_density_2d.html)
draw each line at the height of its level. A layer without a z mapping
is drawn at z = 0, and the z scale extends to include 0. Here the path
is drawn a second time with `inherit.aes = FALSE`, so it lies flat at
height 0 below the helix.

``` r

h + geom_path(colour = PAL["blue"], linewidth = 0.8) +
  geom_path(aes(x, y), inherit.aes = FALSE, colour = PAL["grey"],
            linewidth = 0.8) +
  coord_3d()
```

![The blue helix above a grey circle drawn at height zero; the z axis
now starts at zero.](combining-2d-3d_files/figure-html/z-zero-1.svg)

## Projecting points onto the floor and walls

A position of `-Inf` or `Inf` places a mark on the matching face of the
box, as `Inf` places a mark on the panel edge in ggplot2. The cloud
below has 40 points whose height follows `x + y` with noise.

``` r

set.seed(1)
cloud <- data.frame(x = rnorm(40), y = rnorm(40))
cloud$z <- cloud$x + cloud$y + rnorm(40, sd = 0.5)
```

Mapping `z = -Inf` draws each point’s shadow on the floor, and a
dropline joins each point to it. Mapping `y = Inf` draws the projection
onto the back wall at the largest y, where the points show z against x.

``` r

k <- ggplot(cloud, aes(x, y, z = z))
k + geom_point_3d(aes(z = -Inf), colour = PAL["grey"], size = 1.5) +
  geom_point_3d(aes(y = Inf), colour = PAL["orange"], size = 1.5) +
  geom_dropline_3d(face = "zmin", colour = PAL["grey"]) +
  geom_point_3d(size = 2.5) +
  coord_3d()
```

![Forty dark points in a box, grey shadows on the floor joined to them
by vertical lines, and orange projections on the back
wall.](combining-2d-3d_files/figure-html/projections-1.svg)

Shadows and wall projections use the same x, y and z scales as the
points, so each projection lies on the box grid at the coordinates it
shares with its point. The scales are trained on the finite positions
only, so the box keeps the range of the data.

## Contours and densities on the floor

The surface below has a peak and a hollow. `surface` contains one row
for each of 25 by 25 grid positions with the height `z`.

``` r

surface <- expand.grid(x = seq(-2, 2, length.out = 25),
                       y = seq(-2, 2, length.out = 25))
surface$z <- with(surface, exp(-((x - 0.5)^2 + y^2)) -
  0.6 * exp(-((x + 0.8)^2 + (y + 0.6)^2) / 0.5))
```

[`geom_contour_filled_3d()`](https://gillescolling.com/ggplot3d/reference/geom_contour_3d.md)
with `plane = "floor"` computes height bands from the same grid and
draws them flat on the floor, giving a contour map under the surface. A
higher camera elevation shows more of the floor.

``` r

ramp <- c("#DCEAF3", PAL[["blue"]])
s <- ggplot(surface, aes(x, y, z = z))
s + geom_contour_filled_3d(plane = "floor", bins = 6) +
  geom_surface_3d(fill = "grey85") +
  scale_fill_gradient(low = ramp[1], high = ramp[2]) +
  coord_3d(elevation = 40)
```

![A grey surface with a peak and a hollow above a floor coloured in six
height bands, light for low and dark blue for
high.](combining-2d-3d_files/figure-html/floor-contours-1.svg)

The bands are computed in the same z units as the surface, so the
darkest band on the floor sits under the peak and the lightest under the
hollow.

A density surface can stand over the observations it was estimated from.
`faithful` records the duration and the waiting time of 272 eruptions of
the Old Faithful geyser.
[`stat_density_2d_3d()`](https://gillescolling.com/ggplot3d/reference/stat_density_2d_3d.md)
draws the estimated density, and the eruptions are drawn on the floor
with `z = -Inf`. The surface is half transparent and the camera is
raised, so the points under the two peaks show through. The z title is
set with [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html)
because the density layer computes z and has no z mapping to name it.

``` r

ggplot(faithful, aes(eruptions, waiting)) +
  stat_density_2d_3d(n = 24, alpha = 0.5) +
  geom_point_3d(aes(z = -Inf), size = 1) +
  labs(z = "density") +
  coord_3d(elevation = 50)
```

![A half-transparent grey density surface with two peaks over the
faithful data, with the 272 eruptions drawn as black points on the floor
below the peaks.](combining-2d-3d_files/figure-html/density-floor-1.svg)

## A 3D panel beside 2D panels

A ggplot3d plot is a ggplot object, so patchwork arranges it with 2D
plots. The surface is coloured by height and stacked above a 2D raster
of the same grid. Both plots use the same fill scale, so
`guides = "collect"` draws one legend for both.

``` r

heights <- scale_fill_gradient2(low = PAL["red"], mid = "#F2F2F2",
  high = PAL["blue"], limits = c(-0.6, 1), name = "z")
top <- s + stat_surface_3d(aes(fill = after_stat(z_mean))) +
  heights + coord_3d()
map <- ggplot(surface, aes(x, y, fill = z)) + geom_raster() +
  heights + coord_equal()
top / map + plot_layout(guides = "collect") &
  theme(legend.position = "right")
```

![Top, the surface coloured from red in the hollow to blue at the peak;
bottom, the same grid as a flat raster map in the same colours; one
shared colour bar for
height.](combining-2d-3d_files/figure-html/shared-scale-1.svg)

The 3D panel colours each triangle by its mean height, and the raster
colours each grid cell by its height. The colours agree because the two
plots share the limits of `heights`.

A profile shows one line of the surface at full resolution. `profile`
contains the grid rows with y = 0. The same rows are drawn as a red path
on the 3D surface and as a 2D line against x.

``` r

profile <- surface[abs(surface$y) < 1e-9, ]
across <- s + geom_surface_3d(fill = "grey85") +
  geom_path_3d(data = profile, colour = PAL["red"], linewidth = 1) +
  coord_3d()
side <- ggplot(profile, aes(x, z)) +
  geom_line(colour = PAL["red"], linewidth = 1)
across / side
```

![Top, the grey surface with a red line running across it at y equal to
zero; bottom, the same red line as a 2D profile of z against
x.](combining-2d-3d_files/figure-html/profile-1.svg)

The x axis has the same range in both panels, so the hollow near x = -1
and the peak near x = 0.5 appear at the same x in the path and in the
profile.

## Two camera views of one plot

Facets in one plot share one
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
and therefore one camera. To show the same data from two directions,
give each plot its own
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
and arrange them with patchwork.

``` r

base <- s + geom_surface_3d(fill = "grey85")
(base + coord_3d()) /
  (base + coord_3d(azimuth = 120, elevation = 55))
```

![The grey surface from the default view above the same surface seen
from a higher camera on the opposite
side.](combining-2d-3d_files/figure-html/two-views-1.svg)

[Camera and
framing](https://gillescolling.com/ggplot3d/articles/camera.md)
describes the angles. [The z
scale](https://gillescolling.com/ggplot3d/articles/z-scale.md) shows
facets with fixed and free z ranges.

## Titles and captions

Titles, subtitles and captions are drawn outside the panel in page
coordinates, as in any ggplot2 plot.

``` r

base + coord_3d() +
  labs(title = "Two Gaussian features",
       subtitle = "Peak near (0.5, 0), hollow near (-0.8, -0.6)",
       caption = "Heights on a 25 by 25 grid")
```

![The grey surface with the title Two Gaussian features, a subtitle
giving the positions of the peak and the hollow, and a caption below the
panel.](combining-2d-3d_files/figure-html/titles-1.svg)

In a patchwork layout the same
[`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) call names
one panel, and
[`plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html)
adds a title over the whole arrangement.

``` r

across / side + plot_annotation(title = "Profile at y = 0")
```

## Where next

- [Layers](https://gillescolling.com/ggplot3d/articles/layers.md): every
  three-dimensional geom and stat.

- [Visibility](https://gillescolling.com/ggplot3d/articles/visibility.md):
  how overlapping layers are sorted by depth.

- [Camera and
  framing](https://gillescolling.com/ggplot3d/articles/camera.md): set
  the view and keep its bounds stable.
