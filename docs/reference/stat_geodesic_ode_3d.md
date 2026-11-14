# Equatorial Schwarzschild spacetime geodesics

Integrates u”+u=M/L^2+3 M u^2, u=1/r, using fourth-order Runge-Kutta in
azimuth phi. Null geodesics omit M/L^2. Units are G=c=1. Integration
ends at the finite radial limits, locating the first crossing by
bisection. Computed invariant is u'^2+u^2-2 M u^3-2 M u/L^2 (last term
omitted for null paths). Refine n to check convergence and invariant
conservation. With embedding=TRUE the spatial orbit is projected onto
Flamm's upper paraboloid. These curves are spacetime geodesics projected
onto a spatial slice; the paraboloid's own geodesics use
[`stat_parametric_geodesic_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_geodesic_3d.md).
Use the raster backend for dense orbits on a filled embedding diagram.

## Usage

``` r
stat_geodesic_ode_3d(
  mapping = NULL,
  data = NULL,
  geom = ggplot2::GeomPath,
  position = "identity",
  ...,
  M = 1,
  L = 4,
  null = FALSE,
  u0 = 0.1,
  du0 = 0,
  phi = c(0, 8 * pi),
  n = 1000L,
  r_range = c(2 * M, 100 * M),
  embedding = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- M:

  Positive mass.

- L:

  Positive specific angular momentum, used for timelike paths.

- null:

  Whether the geodesic is null.

- u0, du0:

  Initial inverse radius and its derivative with respect to phi.

- phi:

  Increasing azimuth range in radians.

- n:

  Number of RK4 steps over phi.

- r_range:

  Finite radial domain, at or outside the horizon r=2M.

- embedding:

  Project the orbit onto the spatial embedding diagram.

## Value

A path layer with phi, r, u, du and invariant computed variables.

## References

Sadegh, Kiani and Nouri-Zonoz (2024).
<https://arxiv.org/abs/2308.03104>.

## Examples

``` r
ggplot2::ggplot()+stat_geodesic_ode_3d(M=1,L=4,u0=.1,n=300)+coord_3d()
```
