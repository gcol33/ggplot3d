# Compose a three-axis ordination biplot

Site scores become points; species or variable scores become
solid-headed quiver arrows from the origin. Optional group regions use
the existing ellipsoid or convex-hull statistic. For prcomp, the
coordinates are `x` and `rotation`. For vegan fits, extraction and
scaling use
[`vegan::scores()`](https://vegandevs.github.io/vegan/reference/scores.html).
A named list returned by that function can also be supplied directly.
Each arrow-tip label touches its tip on the side the arrow points to on
the page, as
[`vegan::ordiArrowTextXY()`](https://vegandevs.github.io/vegan/reference/ordiArrowTextXY.html)
places biplot labels, and like all text it is drawn over the scene.

## Usage

``` r
stat_biplot_3d(
  model,
  choices = 1:3,
  groups = NULL,
  region = c("none", "ellipsoid", "hull"),
  loading_scale = 1,
  point_size = 1.5,
  label_loadings = TRUE,
  label_size = 4,
  arrow_colour = "#5C6166",
  level = 0.95,
  scores_args = list()
)
```

## Arguments

- model:

  A prcomp fit, a vegan ordination fit, or a list with sites and
  optional species score matrices.

- choices:

  Three ordination axes.

- groups:

  Named group vector, keyed by site-score row names, or a data frame
  with id and group columns. Its row order is arbitrary.

- region:

  `"none"`, `"ellipsoid"` or `"hull"`.

- loading_scale:

  Explicit multiplier of variable/species scores.

- point_size:

  Site marker size in millimetres.

- label_loadings:

  Whether to label arrow tips.

- label_size:

  Arrow-tip label size, in ggplot2 text size units (mm).

- arrow_colour:

  Colour of the arrow shafts and their unshaded cone heads.

- level:

  Ellipsoid coverage level.

- scores_args:

  Additional arguments to vegan's scores method.

## Value

A list of layers to add to a ggplot with coord_3d().

## Examples

``` r
fit <- stats::prcomp(iris[1:4], scale. = TRUE)
groups <- stats::setNames(iris$Species, rownames(iris))
ggplot2::ggplot() + stat_biplot_3d(fit, groups = groups,
  region = "ellipsoid", loading_scale = 2) + coord_3d()
```
