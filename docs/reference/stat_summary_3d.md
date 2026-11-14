# Summarise one axis at each position on the other two

For every group and every distinct pair of positions on the two other
axes, summarises the values along `axis` into a centre and an interval,
as
[`ggplot2::stat_summary()`](https://ggplot2.tidyverse.org/reference/stat_summary.html)
does for y at each x. The summary function returns columns `y`, `ymin`
and `ymax` (as
[`ggplot2::mean_se()`](https://ggplot2.tidyverse.org/reference/mean_se.html)
and the Hmisc wrappers such as
[`ggplot2::mean_cl_normal()`](https://ggplot2.tidyverse.org/reference/hmisc.html)
do); they become `z`, `zmin` and `zmax`, or the matching columns of
`axis`. Summaries are computed on the transformed scale.

## Usage

``` r
stat_summary_3d(
  mapping = NULL,
  data = NULL,
  geom = "pointrange",
  position = "identity",
  ...,
  fun.data = NULL,
  fun = NULL,
  fun.max = NULL,
  fun.min = NULL,
  fun.args = list(),
  axis = "z",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geometry drawing the summary: `"pointrange"`, `"errorbar"`,
  `"linerange"`, `"crossbar"`, `"point"`, `"path"`, `"line"`, or a
  registered geom prototype.

- fun.data:

  Function, or its name, taking a numeric vector and returning a data
  frame with any of `y`, `ymin` and `ymax`. Defaults to
  [`ggplot2::mean_se()`](https://ggplot2.tidyverse.org/reference/mean_se.html)
  when no summary function is given.

- fun, fun.min, fun.max:

  Functions, or names, each returning one number: the centre, lower and
  upper bound. Used when `fun.data` is `NULL`.

- fun.args:

  List of further arguments to the summary functions.

- axis:

  Axis to summarise: `"z"`, `"x"` or `"y"`.

## Value

A ggplot2 layer.

## Examples

``` r
set.seed(1)
d <- expand.grid(x = 1:3, y = 1:2, rep = 1:10)
d$z <- d$x + d$y + rnorm(nrow(d), sd = 0.5)
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
p + stat_summary_3d() + coord_3d()
p + stat_summary_3d(geom = "errorbar", fun = median,
  fun.min = function(z) quantile(z, 0.25), fun.max = function(z) quantile(z, 0.75),
  width = 0.2) + coord_3d()
p + stat_summary_3d(geom = "crossbar", fun.data = ggplot2::mean_se,
  fun.args = list(mult = 2), width = 0.3, cap.plane = "cross", fill = "grey90") + coord_3d()
```
