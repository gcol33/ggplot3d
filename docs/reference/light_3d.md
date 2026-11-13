# Lighting for a 3D scene

Lighting for a 3D scene

## Usage

``` r
light_3d(
  direction = c(-1, 1, 2),
  space = "camera",
  ambient = 0.35,
  diffuse = 0.65,
  mode = "multiply"
)
```

## Arguments

- direction:

  Direction from a surface toward the light.

- space:

  Coordinate space, camera or data.

- ambient, diffuse:

  Nonnegative light contributions with sum at most one.

- mode:

  Colour operation, multiply or tint.

## Value

A lighting configuration for coord_3d().

## Examples

``` r
light_3d(direction = c(1, 1, 2), space = "camera")
```
