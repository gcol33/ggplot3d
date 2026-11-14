# Reference planes

Draws planes through the panel box: planes of constant x, y or z, the
three-dimensional counterparts of
[`ggplot2::geom_hline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html)
and
[`ggplot2::geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html),
and planes of any orientation, such as a fitted regression plane. Each
plane is cut to the panel box and drawn as one flat face, so it
intersects surfaces and solids exactly.

## Usage

``` r
geom_plane_3d(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  xintercept,
  yintercept,
  zintercept,
  normal = NULL,
  point = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- xintercept, yintercept, zintercept:

  Values at which to draw planes of constant x, y or z. Given here, they
  replace the layer's data and mapping, as in
  [`ggplot2::geom_hline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html).

- normal, point:

  A plane of any orientation: its normal along x, y and z in the units
  of the transformed position scales, and a point on it in data units,
  as in
  [`clip_plane_3d()`](https://gillescolling.com/ggplot3d/reference/clip_plane_3d.md).
  The point does not train the scales; `NULL` takes the centre of the
  panel box.

## Value

A ggplot2 layer.

## Details

Without the arguments above, each row of the data gives one plane
through the aesthetics `xintercept`, `yintercept` or `zintercept`, or
through the point `x`, `y`, `z` with normal `nx`, `ny`, `nz` (in the
units of the transformed position scales).

The plane z = a + b x + c y has normal `c(-b, -c, 1)` and passes through
`c(0, 0, a)`.

## Aesthetics

`xintercept`, `yintercept`, `zintercept`, `x`, `y`, `z`, `nx`, `ny`,
`nz`, `fill`, `alpha`, `colour` (the outline where the plane meets the
box), `linewidth`, `linetype`.

## See also

[`clip_plane_3d()`](https://gillescolling.com/ggplot3d/reference/clip_plane_3d.md)
cuts layers by a plane.

## Examples

``` r
d <- expand.grid(x = seq(-2, 2, length.out = 30), y = seq(-2, 2, length.out = 30))
d$z <- exp(-(d$x^2 + d$y^2))
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_surface_3d(fill = "grey85")
p + geom_plane_3d(zintercept = 0.5, fill = "steelblue") + coord_3d()
p + geom_plane_3d(xintercept = c(-1, 1), colour = "grey20") + coord_3d()

fit <- stats::lm(mpg ~ wt + hp, data = mtcars)
b <- stats::coef(fit)
ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp, z = mpg)) +
  geom_point_3d() +
  geom_plane_3d(normal = c(-b[["wt"]], -b[["hp"]], 1), point = c(0, 0, b[[1]])) +
  coord_3d()
```
