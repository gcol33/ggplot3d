# Intervals along x, y and z: error bars, line ranges, point ranges and crossbars

Each row draws a bar along every axis for which it has both bounds:
`xmin`/`xmax`, `ymin`/`ymax`, `zmin`/`zmax`, in any combination. A row
with both bounds missing on an axis draws no bar there.
`geom_errorbar_3d()` adds caps at both ends, `geom_pointrange_3d()` a
point at `x`, `y`, `z`, and `geom_crossbar_3d()` a box spanning the
interval with a line at the centre. A missing centre is taken as the
middle of the interval; a layer without z lies on the floor of the box.

## Usage

``` r
geom_errorbar_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  width = NULL,
  cap.plane = "auto",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_linerange_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_pointrange_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_crossbar_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  width = NULL,
  cap.plane = "auto",
  fatten = 2.5,
  middle.colour = NULL,
  middle.color = NULL,
  middle.linetype = NULL,
  middle.linewidth = NULL,
  box.colour = NULL,
  box.color = NULL,
  box.linetype = NULL,
  box.linewidth = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments. Positions:
  [`position_nudge_3d()`](https://gillescolling.com/ggplot3d/reference/position_nudge_3d.md),
  [`position_jitter_3d()`](https://gillescolling.com/ggplot3d/reference/position_jitter_3d.md)
  and
  [`position_dodge_3d()`](https://gillescolling.com/ggplot3d/reference/position_dodge_3d.md).

- width:

  Cap and box width in transformed data units of the axis it extends
  along. Defaults to 0.9 times the spacing of the positions along that
  axis.

- cap.plane:

  Plane of the caps: `"auto"`, `"xy"`, `"xz"`, `"yz"` or `"cross"`.

- fatten:

  Factor applied to `linewidth` for the crossbar's middle line.

- middle.colour, middle.color, middle.linetype, middle.linewidth,
  box.colour, box.color, box.linetype, box.linewidth:

  Stroke of the crossbar's middle line and box, overriding the layer's
  aesthetics.

## Value

A ggplot2 layer.

## Details

Caps and crossbar boxes are perpendicular to their bar and lie in the
plane named by `cap.plane`. A bar lying in that plane is capped along
the plane's other axis. A bar normal to the plane, or any bar with
`"cross"`, is capped along both perpendicular axes: a cross for error
bars and a cuboid for crossbars, which read the same from every view.
With `"auto"`, bars along z are capped along x and bars along x or y
within the floor plane.

The registered ggplot2 layers
[`ggplot2::geom_errorbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html),
[`ggplot2::geom_linerange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html),
[`ggplot2::geom_pointrange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
and
[`ggplot2::geom_crossbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html)
draw their x and y intervals the same way inside
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md),
on the floor unless z is mapped; z intervals need the layers documented
here.

## See also

[`stat_summary_3d()`](https://gillescolling.com/ggplot3d/reference/stat_summary_3d.md)
computes intervals from data,
[`geom_stem_3d()`](https://gillescolling.com/ggplot3d/reference/geom_stem_3d.md)
draws stems to a base value.

## Examples

``` r
d <- expand.grid(x = 1:3, y = 1:2)
d$z <- c(2, 3, 5, 4, 6, 5)
d$se <- c(0.4, 0.6, 0.5, 0.8, 0.3, 0.7)
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, zmin = z - se, zmax = z + se))
p + geom_errorbar_3d(width = 0.2) + geom_point_3d() + coord_3d()
p + geom_pointrange_3d(colour = "steelblue") + coord_3d()
p + geom_crossbar_3d(width = 0.3, fill = "grey85", cap.plane = "cross") + coord_3d()

# Intervals on all three axes at once
e <- data.frame(x = c(1, 2), y = c(1, 3), z = c(2, 1))
ggplot2::ggplot(e, ggplot2::aes(x, y, z = z,
    xmin = x - 0.3, xmax = x + 0.3, ymin = y - 0.5, ymax = y + 0.5,
    zmin = z - 0.4, zmax = z + 0.4)) +
  geom_errorbar_3d(width = 0.15, cap.plane = "cross") + coord_3d()

# Ranges on the floor, from x/y alone
ggplot2::ggplot(e, ggplot2::aes(x, y, ymin = y - 1, ymax = y + 1)) +
  geom_linerange_3d(linewidth = 1) + coord_3d()
```
