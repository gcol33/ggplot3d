# Lights for a 3D scene

Lights combine in
[`light_3d()`](https://gillescolling.com/ggplot3d/reference/light_3d.md),
which
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
takes as `light`, with an ambient term. Each face colour is lit by one
shading kernel shared by the vector and raster backends and the
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
viewer: the Blinn-Phong model (Phong 1975, Blinn 1977) in linear light,
with the ambient, diffuse and specular strengths of the face's
[`material_3d()`](https://gillescolling.com/ggplot3d/reference/material_3d.md).
Lights follow the light objects of MATLAB (`light`,
`camlight headlight`), rgl (`light3d()`), plotly (`lightposition`) and
three.js (`DirectionalLight`, `PointLight`, `HemisphereLight`).

## Usage

``` r
light_environment(image, intensity = 1, rotation = 0, space = "data")

light_directional(
  direction = c(-1, 1, 2),
  space = "camera",
  colour = "white",
  intensity = 0.65
)

light_point(
  position,
  space = "data",
  colour = "white",
  intensity = 1,
  falloff = 2
)

light_head(colour = "white", intensity = 0.65)

light_hemisphere(
  sky = "white",
  ground = "grey30",
  direction = c(0, 0, 1),
  space = "data",
  intensity = 0.35
)
```

## Arguments

- image:

  Linear RGB array with dimensions height by width by 3, or a Radiance
  RGBE `.hdr` or sRGB `.png` path. PNG input needs package png.

- intensity:

  Nonnegative multiplier of the light colour.

- rotation:

  Environment rotation about the up axis, in degrees.

- space:

  `"camera"` or `"data"`.

- direction:

  Direction from a surface toward the light, or the sky direction of a
  hemisphere light.

- colour, sky, ground:

  Light colours.

- position:

  Position of a point light.

- falloff:

  Nonnegative exponent of the distance falloff.

## Value

A light for
[`light_3d()`](https://gillescolling.com/ggplot3d/reference/light_3d.md).

## Details

`light_directional()` shines along a fixed direction from infinitely far
away. `light_point()` sits at a position and its contribution falls off
as `intensity / d^falloff`, with `d` the distance in panel units (the
box side is one, stretched by the camera aspect) and at least 0.001;
`falloff = 2` is the inverse-square law, `falloff = 0` a constant. The
falloff never depends on the other geometry in the panel. `light_head()`
is a directional light that shines from the eye, like MATLAB's
`camlight headlight`. `light_hemisphere()` adds an ambient term that
blends from `ground` to `sky` as the normal turns from `-direction` to
`direction` (three.js `HemisphereLight`). `light_environment()` supplies
an equirectangular radiance map. The first row is the north pole;
columns run around the up axis from positive x toward positive y.
Rotation is in degrees about that axis. The path tracer samples the map
by luminance and solid angle. Other backends use its
solid-angle-weighted mean as ambient illumination.

In `space = "camera"`, x points right, y up and z toward the viewer, and
positions are relative to the camera target in panel units. In
`space = "data"`, directions are in the normalised panel axes and
positions in data units.

## References

Phong (1975). Illumination for computer generated pictures.
Communications of the ACM 18, 311-317.
[doi:10.1145/360825.360839](https://doi.org/10.1145/360825.360839)

Blinn (1977). Models of light reflection for computer synthesized
pictures. SIGGRAPH '77, 192-198.
[doi:10.1145/563858.563893](https://doi.org/10.1145/563858.563893)

## Examples

``` r
light_3d(light_directional(c(1, 1, 2)), light_point(c(0, 0, 3), falloff = 2))
```
