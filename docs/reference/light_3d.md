# Lighting for a 3D scene

One directional light with an ambient term: the shorthand for
`lights_3d(light_directional(direction, space, intensity = diffuse), ambient = ambient, mode = mode, ...)`.
See
[`lights_3d()`](https://gillescolling.com/ggplot3d/reference/lights_3d.md)
for several lights, point, head and hemisphere lights and the colour
modes, and
[`material_3d()`](https://gillescolling.com/ggplot3d/reference/material_3d.md)
for specular highlights.

## Usage

``` r
light_3d(
  direction = c(-1, 1, 2),
  space = "camera",
  ambient = 0.35,
  diffuse = 0.65,
  mode = "multiply",
  ...
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

  Colour operation: `"multiply"`, `"tint"`, `"lightness"`, `"tone"`,
  `"overlay"` or `"soft"`; see
  [`lights_3d()`](https://gillescolling.com/ggplot3d/reference/lights_3d.md).

- ...:

  `range`, `neutral`, `shadow` and `highlight` of
  [`lights_3d()`](https://gillescolling.com/ggplot3d/reference/lights_3d.md).

## Value

A lighting configuration for coord_3d().

## Examples

``` r
light_3d(direction = c(1, 1, 2), space = "camera")
light_3d(mode = "lightness", range = c(-0.5, 0.2))
```
