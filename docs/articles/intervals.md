# Displaying intervals and summaries

Intervals show supplied lower and upper bounds around a position. They
can describe measurement uncertainty, variation among observations or
uncertainty in a summary, depending on how the bounds were computed.
This article draws intervals on three axes and summarises petal lengths
by species. Installation is covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md).
[Positioning groups and
observations](https://gillescolling.com/ggplot3d/articles/positions.md)
separates groups sharing a position.

1.  **Map** the centre and both bounds of each interval axis in
    [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

2.  **Draw** capped bars, point ranges or boxes with
    [`geom_errorbar_3d()`](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md),
    [`geom_pointrange_3d()`](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md)
    or
    [`geom_crossbar_3d()`](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md).

3.  **Summarise** repeated observations with
    [`stat_summary_3d()`](https://gillescolling.com/ggplot3d/reference/stat_summary_3d.md).

## Example

``` r

library(ggplot2)
library(ggplot3d)
```

### Drawing supplied bounds

Each row below describes a measured position and a stated half-width on
every axis, in arbitrary units.
[geom_errorbar_3d()](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md)
draws a bar for each pair of bounds. The example widths are supplied
values; the layer draws them without estimating their statistical
meaning.

``` r

measurements <- data.frame(x = c(1, 2, 3), y = c(2, 1, 3),
                           z = c(3, 4, 2))
ranges <- ggplot(measurements, aes(x, y, z = z,
  xmin = x - 0.2, xmax = x + 0.2,
  ymin = y - 0.3, ymax = y + 0.3,
  zmin = z - 0.5, zmax = z + 0.5))
ranges + geom_errorbar_3d(width = 0.12, cap_plane = "cross",
                           linewidth = 0.8) +
  geom_point_3d(size = 3, colour = "#2E6F9E") + coord_3d()
```

![Three measured points with capped intervals along x, y and
z.](intervals_files/figure-html/supplied-1.svg)

Cross-shaped caps extend in both directions perpendicular to each bar.
The width is measured in transformed coordinate units along those
directions. For a single z interval, `cap_plane = "xz"` puts the caps
along x. A missing pair of bounds omits that axis; supply both endpoints
for every interval that should appear.

| Geometry | Display |
|----|----|
| [`geom_linerange_3d()`](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md) | Interval segments |
| [`geom_errorbar_3d()`](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md) | Segments with endpoint caps |
| [`geom_pointrange_3d()`](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md) | Segments and a centre point |
| [`geom_crossbar_3d()`](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md) | Boxes with a line at the centre |

The same mappings can draw point ranges. Use them when the centre and
extent are enough to read the interval. Joint uncertainty across axes
needs a model of their dependence; three separate marginal intervals do
not define a joint confidence region.

``` r

ranges + geom_pointrange_3d(colour = "#2E6F9E", linewidth = 0.8) +
  coord_3d()
```

![The same three positions shown as points with uncapped intervals on
all axes.](intervals_files/figure-html/point-ranges-1.svg)

### Summarising repeated observations

The iris table records petal lengths in centimetres for three species.
[stat_summary_3d()](https://gillescolling.com/ggplot3d/reference/stat_summary_3d.md)
summarises z within each group and each distinct x/y position. Its
default summary is the mean plus or minus one standard error, calculated
from the observations in that cell.

``` r

flowers <- ggplot(iris, aes(Species, y = 0, z = Petal.Length)) +
  scale_y_continuous(limits = c(-1, 1), breaks = NULL) +
  scale_x_discrete(guide = guide_axis_3d(n.dodge = 2))
flowers + stat_summary_3d(fun.data = mean_se, colour = "#2E6F9E",
                          linewidth = 0.9) +
  labs(z = "Petal length (cm)", y = NULL) + coord_3d()
```

![Mean petal length with one-standard-error intervals for each iris
species.](intervals_files/figure-html/means-1.svg)

One standard error describes precision of the sample mean. To display
the spread of observations, supply a summary such as the median with the
first and third quartiles. The function returns columns named y, ymin
and ymax, which the stat translates to the selected axis.

``` r

quartiles <- function(x) {
  q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
  data.frame(y = q[2], ymin = q[1], ymax = q[3])
}
flowers + stat_summary_3d(fun.data = quartiles, geom = "crossbar",
                          width = 0.35, fill = "#2E6F9E") +
  labs(z = "Petal length (cm)", y = NULL) + coord_3d()
```

![Median petal length with boxes spanning the first to third quartiles
for each iris species.](intervals_files/figure-html/quartiles-1.svg)

An explicit group mapping separates summaries that share a position.
Discrete colour and fill mappings also define groups. Mapping a
different group to every observation prevents pooling; a standard error
then cannot be estimated from within-cell variation. Summaries use
scale-transformed values, so applying a logarithmic scale changes the
quantity being averaged. Compute summaries in original units before
plotting when that is the intended quantity.

A cell with a single observation has no standard error. Its mean is
drawn as a point without bounds.

### Preparing a summary table

A precomputed table makes the bounds available for checking and reuse.
The following table contains one row per species, with the mean and
one-standard-error bounds in centimetres. Its named columns feed
directly into the point-range layer.

``` r

summary_table <- do.call(rbind, lapply(split(iris, iris$Species), function(d) {
  mean_length <- mean(d$Petal.Length)
  se <- sd(d$Petal.Length) / sqrt(nrow(d))
  data.frame(species = d$Species[1], mean = mean_length,
             lower = mean_length - se, upper = mean_length + se)
}))
rownames(summary_table) <- NULL
summary_table
#>      species  mean    lower    upper
#> 1     setosa 1.462 1.437440 1.486560
#> 2 versicolor 4.260 4.193545 4.326455
#> 3  virginica 5.552 5.473950 5.630050
```

``` r

ggplot(summary_table, aes(species, y = 0, z = mean,
                          zmin = lower, zmax = upper)) +
  geom_pointrange_3d(colour = "#2E6F9E", linewidth = 0.9) +
  scale_y_continuous(limits = c(-1, 1), breaks = NULL) +
  scale_x_discrete(guide = guide_axis_3d(n.dodge = 2)) +
  labs(z = "Petal length (cm)", y = NULL) + coord_3d()
```

![The precomputed species means and standard-error bounds drawn as point
ranges.](intervals_files/figure-html/precomputed-1.svg)

[Positioning groups and
observations](https://gillescolling.com/ggplot3d/articles/positions.md)
shows how to separate groups within a cell. For fitted-mean uncertainty
and observation-to-fit segments, use the model examples in [Building
surfaces and
solids](https://gillescolling.com/ggplot3d/articles/surfaces-solids.html#function-and-fitted-surfaces).
Its ellipsoid examples describe three-dimensional spread and uncertainty
in a mean.
