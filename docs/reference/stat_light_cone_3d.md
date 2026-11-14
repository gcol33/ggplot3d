# Light cones and surfaces of constant spacetime interval

Draws c^2 (t-t0)^2 - (x-x0)^2 - (y-y0)^2 = interval with time along z.
The zero interval gives light cones, positive intervals timelike
hyperboloids, and negative intervals spacelike hyperboloids. Map x, y
and t for a cone at each event, or supply apex. Spatial extent is radial
for nonnegative interval and temporal for negative interval. Timelike
shells compute klein_x and klein_y as v/c, and rapidity from the rest
event.

## Usage

``` r
stat_light_cone_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  apex = c(0, 0, 0),
  extent = 2,
  interval = 0,
  speed = 1,
  sheet = c("both", "future", "past"),
  n = 30L,
  boost = NULL,
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

- apex:

  Event (x,y,t) when no event aesthetics are mapped.

- extent:

  Positive radial or temporal parameter extent.

- interval:

  Signed squared spacetime interval.

- speed:

  Positive speed of light in the chosen units.

- sheet:

  Both, future or past sheets.

- n:

  Number of samples along u and v, scalar or two integers.

- boost:

  Optional 3 by 3
  [`lorentz_boost_3d()`](https://gillescolling.com/ggplot3d/reference/lorentz_boost_3d.md)
  in (x,y,c\*t).

## Value

A surface layer.

## Examples

``` r
ggplot2::ggplot()+stat_light_cone_3d(extent=2,n=16,alpha=.4)+coord_3d()
ggplot2::ggplot()+stat_light_cone_3d(interval=1,sheet="future",n=16)+coord_3d()
```
