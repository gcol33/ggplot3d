# Scaling heights and facet ranges

The z scale maps heights from data units into panel positions and
controls their transformations, categories, limits and facets. Set
`coord_3d(z_scales = "free")` for separate z ranges across facets.
ggplot2 facets have no z scale. Installation is covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md). For
repeated camera views, see [Choosing camera
views](https://gillescolling.com/ggplot3d/articles/camera.md);
[Combining 2D and 3D
graphics](https://gillescolling.com/ggplot3d/articles/combining-2d-3d.html#small-multiples-in-depth)
stacks two-dimensional panels in depth.

``` r

library(ggplot2)
library(ggplot3d)
```

## Transforming heights

A position scale trains its range from layer data and supplies breaks
and labels. The z position family includes z, zmin, zmax, zend and
zintercept. A logarithmic transformation therefore also applies to the
endpoints of segments.

``` r

d <- data.frame(x = 1:3, y = c(1, 2, 1), z = c(1, 10, 100),
                top = c(10, 100, 1000))
ggplot(d, aes(x, y, z = z, xend = x, yend = y, zend = top)) +
  geom_segment_3d(linewidth = 0.8, colour = "#2E6F9E") +
  scale_z_log10(breaks = c(1, 10, 100, 1000)) + coord_3d()
```

![Three vertical segments with endpoints on a logarithmic height
scale.](z-scale_files/figure-html/log-1.svg)

Use positive values with a logarithmic scale.
[`scale_z_sqrt()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md)
compresses large values less strongly, and
[`scale_z_reverse()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md)
turns the axis upside down, which suits depths below a surface.

``` r

ggplot(d, aes(x, y, z = z)) + geom_point_3d(size = 3, colour = "#2E6F9E") +
  scale_z_reverse(name = "depth (m)") + coord_3d()
```

![Three points at depths of one, ten and one hundred metres, with the
deepest point at the bottom of a reversed
axis.](z-scale_files/figure-html/reverse-1.svg)

Other continuous transformations go through the `transform` argument of
[`scale_z_continuous()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md).

## Categories and dates

A discrete scale places categories at separate z positions in
factor-level order. Dates and date-times use their corresponding scale
constructors and accept date breaks and labels.

``` r

categories <- data.frame(x = 1:3, y = c(1, 2, 1),
  stage = factor(c("low", "middle", "high"), levels = c("low", "middle", "high")))
ggplot(categories, aes(x, y, z = stage)) + geom_point_3d(size = 3) +
  scale_z_discrete(name = "Stage") + coord_3d()
```

![Three points at the low, middle and high category
levels.](z-scale_files/figure-html/categories-1.svg)

``` r

dated <- data.frame(x = 1:3, y = c(1, 2, 1),
                    day = as.Date("2026-01-01") + 0:2)
ggplot(dated, aes(x, y, z = day)) + geom_point_3d(size = 3) +
  scale_z_date(date_labels = "%d %b") + coord_3d()
```

![Three points at dates spaced one day
apart.](z-scale_files/figure-html/dates-1.svg)

Durations such as elapsed hours use
[`scale_z_time()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md),
which labels the axis as hours, minutes and seconds.

``` r

dated$elapsed <- as.difftime(1:3, units = "hours")
ggplot(dated, aes(x, y, z = elapsed)) + geom_point_3d(size = 3) +
  scale_z_time() + coord_3d()
```

![Three points at one, two and three hours of elapsed
time.](z-scale_files/figure-html/times-1.svg)

## Scale limits and coordinate limits

Scale limits participate in data handling before statistical
computations. Coordinate limits set the displayed box. Set coordinate
limits to inspect part of a surface while retaining the data used to
construct it.

``` r

ggplot(d, aes(x, y, z = z)) + geom_point_3d(size = 3) +
  coord_3d(zlim = c(0, 100))
```

![Points viewed within a z range from zero to one
hundred.](z-scale_files/figure-html/limits-1.svg)

The shorthand
[`zlim()`](https://gillescolling.com/ggplot3d/reference/zlim.md)
supplies scale limits in original data units. Coordinate limits also use
original units. [Building surfaces and
solids](https://gillescolling.com/ggplot3d/articles/surfaces-solids.md)
explains the transformed units used by statistical surfaces.

## Facets

Facets divide the data into separate panels. By default panels share a z
range. Set `coord_3d(z_scales = "free")` to train a separate z range per
panel. ggplot2 facets have no z scale; their `scales` argument controls
x and y.

``` r

faceted <- data.frame(x = rep(1:3, 2), y = rep(c(1, 2, 1), 2),
  z = c(1, 2, 3, 20, 40, 60), site = rep(c("a", "b"), each = 3))
overhead <- light_3d(light_directional_3d(c(0, 0, 1), space = "data"))
ggplot(faceted, aes(x, y, z = z)) + geom_point_3d(size = 3) +
  facet_wrap(~site, labeller = label_both) +
  coord_3d(z_scales = "free", shadow = shadow_3d(light = overhead, alpha = 0.5))
```

![Two side-by-side panels with separate z ranges, 1 to 3 for site a and
20 to 60 for site b, each point casting a shadow straight down onto the
floor.](z-scale_files/figure-html/facets-1.svg)

Free x and y ranges remain controlled by ggplot2’s facet arguments. Use
fixed z ranges when equal heights should occupy equal positions across
panels.

## Guides and themes

A guide draws breaks, tick labels and an axis title. The z guide uses an
exterior edge of the projected box. Theme elements for z inherit the
corresponding general axis settings.

``` r

ggplot(d, aes(x, y, z = z)) + geom_point_3d(size = 3) +
  scale_z_continuous(name = "Height", guide = guide_axis_3d(n.dodge = 1)) +
  coord_3d() + theme(axis.title.z = element_text(colour = "#2E6F9E"))
```

![Three points with a Height axis title and a blue z
axis.](z-scale_files/figure-html/guide-1.svg)

[Styling axes, legends and
panes](https://gillescolling.com/ggplot3d/articles/guides-themes.md)
shows edge-following labels, a colourbar aligned with height and theme
elements for individual panes.
