# Integrate streamlines and streamtubes through a vector field

A complete rectilinear x/y/z grid supplies u/v/w components. Trilinear
interpolation and integration run in C++. Integration solves dp/dt =
v(p) in transformed coordinate units; supply components in those units
when using nonlinear position scales. Missing field cells stop a
streamline.

## Usage

``` r
stat_streamline_3d(
  mapping = NULL,
  data = NULL,
  geom = c("path", "tube"),
  position = "identity",
  ...,
  seeds = NULL,
  n = 6L,
  step = 0.05,
  method = c("rk4", "rk45"),
  max_steps = 1000L,
  direction = c("both", "forward", "backward"),
  tolerance = 1e-05,
  max_lines = 200L,
  radius = NULL,
  sides = 12L,
  cap = "round",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  `"path"` for lines or `"tube"` for rotation-minimising streamtubes.

- seeds:

  Numeric x/y/z rows in original position units; a list with `from`,
  `to`, `n` for a line rake; or a list with `plane` (`"xy"`, `"xz"`,
  `"yz"`), `at` and `n` for a plane. NULL seeds a yz plane at the x
  midpoint.
  `list(type = "even", plane = "xy", at = ..., spacing = ..., dtest = ...)`
  uses Jobard-Lefer placement of streamlines of the projected field in
  each slice. The normal component is zero in this mode. Candidate seeds
  are `spacing` from accepted curves; tracing stops within `dtest`
  (default half the spacing). Sampling steps stay below half `dtest`.

- n:

  Samples along a rake or each plane axis; one or two integers.

- step:

  Maximum integration time step, default 0.05.

- method:

  Fixed-step `"rk4"` or adaptive Dormand-Prince `"rk45"`.

- max_steps:

  Maximum accepted steps in each direction from a seed.

- direction:

  `"both"`, `"forward"` or `"backward"`.

- tolerance:

  RK45 local error tolerance relative to each axis span.

- max_lines:

  Maximum curves per plane or explicit seed collection.

- radius, sides, cap:

  Passed to
  [`geom_tube_3d()`](https://gillescolling.com/ggplot3d/reference/geom_tube_3d.md)'s
  sweep machinery.

## Value

A ggplot2 layer with computed `norm`, `step`, `arclength`, `time` and
`group`. Streamtubes are drawn as
[`geom_tube_3d()`](https://gillescolling.com/ggplot3d/reference/geom_tube_3d.md)
draws its tubes, with these variables at their points.

## References

Dormand and Prince (1980), A family of embedded Runge-Kutta formulae.
Jobard and Lefer (1997), Creating evenly-spaced streamlines of arbitrary
density,
[doi:10.1007/978-3-7091-6876-9_5](https://doi.org/10.1007/978-3-7091-6876-9_5)
.

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 5),
                 y = seq(-1, 1, length.out = 5), z = c(-1, 1))
d$u <- -d$y; d$v <- d$x; d$w <- 0.15
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w,
                               colour = ggplot2::after_stat(norm))) +
  stat_streamline_3d(seeds = rbind(c(0.6, 0, -0.8)), max_steps = 80) + coord_3d()
```
