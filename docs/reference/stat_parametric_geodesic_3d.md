# Geodesics on parametric surfaces

Traces geodesics of the surface `fun(u, v)` of
[`stat_parametric_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_3d.md)
from each `start` parameter pair in each direction `angle`, by
integrating the geodesic equation in the parameters. Geodesics are the
trajectories of the Hamiltonian H = 1/2 g^ij p_i p_j, with g the first
fundamental form; a symplectic, symmetric integrator of order four (the
generalised Stoermer-Verlet method composed by Yoshida's triple jump)
keeps the speed, and with it the arc length, accurate over long runs.
Metric derivatives come from central differences of `fun`.

## Usage

``` r
stat_parametric_geodesic_3d(
  mapping = NULL,
  data = NULL,
  geom = ggplot2::GeomPath,
  position = "identity",
  ...,
  fun,
  start,
  angle = 0,
  length = 1,
  n = 200L,
  args = list(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom drawing the geodesics.

- fun:

  Vectorised function of u and v returning x, y and z as a list, data
  frame or three-column matrix.

- start:

  Starting parameters, `c(u, v)` or a two-column matrix.

- angle:

  Starting directions in radians.

- length:

  Arc length of each geodesic.

- n:

  Number of integration steps.

- args:

  Additional named arguments passed to fun.

## Value

A ggplot2 layer.

## Details

`angle` is measured in the tangent plane at the start, in radians, from
the direction of increasing u towards that of increasing v. Each
geodesic is followed for arc length `length` in `n` steps and drawn as a
path. The computed variables are `u`, `v`, `s` (arc length), `start`
(row of `start`) and `angle`. A geodesic that reaches a singular point
of the parametrisation, such as a pole of a sphere, stops the layer with
an error.

## References

Hairer, Lubich and Wanner (2006). Geometric Numerical Integration, 2nd
edition. Springer.
[doi:10.1007/3-540-30666-8](https://doi.org/10.1007/3-540-30666-8)

Yoshida (1990). Construction of higher order symplectic integrators.
Physics Letters A 150, 262-268.
[doi:10.1016/0375-9601(90)90092-3](https://doi.org/10.1016/0375-9601%2890%2990092-3)

## Examples

``` r
torus <- function(u, v) list(x = (1 + 0.4 * cos(v)) * cos(u),
  y = (1 + 0.4 * cos(v)) * sin(u), z = 0.4 * sin(v))
ggplot2::ggplot() +
  geom_parametric_3d(fun = torus, n = c(48, 24), closed = TRUE, fill = "grey85") +
  stat_parametric_geodesic_3d(ggplot2::aes(colour = ggplot2::after_stat(angle)),
    fun = torus, start = c(0, 0), angle = seq(0.2, 1.4, by = 0.3), length = 12) +
  coord_3d()
```
