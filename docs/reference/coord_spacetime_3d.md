# Finite Minkowski spacetime coordinates

Uses x and y as spatial coordinates and z as time. Lorentz isometries
act on (x,y,c\*t). The shared scene adapter projects finite events and
straight world lines; the camera gives an exterior view of the spacetime
diagram. animate_3d(isometry=...) applies a sequence of boosts to the
same scene.

## Usage

``` r
coord_spacetime_3d(speed = 1, ...)
```

## Arguments

- speed:

  Positive speed of light in plot units.

- ...:

  Arguments to
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md),
  including limits and isometry.

## Value

A Coord3D object.

## Examples

``` r
ggplot2::ggplot()+stat_light_cone_3d(n=12)+coord_spacetime_3d()
```
