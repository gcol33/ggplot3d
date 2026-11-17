# Lighting for a 3D scene

Combines light sources with an ambient term and sets how the light
changes face colours. Without light sources the scene has the lights of
`preset`; the default preset has one directional light,
[`light_directional_3d()`](https://gillescolling.com/ggplot3d/reference/light_directional_3d.md),
from the upper left of the camera. See
[`material_3d()`](https://gillescolling.com/ggplot3d/reference/material_3d.md)
for specular highlights.

## Usage

``` r
light_3d(
  ...,
  ambient = NULL,
  mode = NULL,
  range = NULL,
  neutral = NULL,
  shadow_colour = "#1d2b53",
  highlight_colour = "#fff1d6",
  linear = NULL,
  material = NULL,
  viewer = NULL,
  preset = "default"
)
```

## Arguments

- ...:

  Lights made with
  [`light_directional_3d()`](https://gillescolling.com/ggplot3d/reference/light_directional_3d.md),
  [`light_point_3d()`](https://gillescolling.com/ggplot3d/reference/light_directional_3d.md),
  [`light_head_3d()`](https://gillescolling.com/ggplot3d/reference/light_directional_3d.md),
  [`light_hemisphere_3d()`](https://gillescolling.com/ggplot3d/reference/light_directional_3d.md)
  or
  [`light_environment_3d()`](https://gillescolling.com/ggplot3d/reference/light_directional_3d.md).
  None gives the lights of `preset`.

- ambient:

  Uniform ambient light: a nonnegative strength (a grey) or one colour.
  NULL takes the preset's.

- mode:

  Colour operation: `"multiply"`, `"tint"`, `"lightness"`, `"tone"`,
  `"overlay"`, `"soft"` or `"value"`. NULL takes the preset's.

- range:

  Moves at full shadow and full highlight, `c(shadow, highlight)` with
  `-1 <= shadow <= 0 <= highlight <= 1`: fractions of the way to black
  and white in OKLab lightness for `"lightness"` and in HSV value for
  `"value"`, to the tone colours for `"tone"`, blend strengths for
  `"overlay"` and `"soft"`. NULL takes the preset's when `mode` is the
  preset's mode, else `c(-0.45, 0.3)` for `"lightness"` and
  `c(-0.6, 0.6)` otherwise.

- neutral:

  Angle in degrees, in \[0, 180), between the normal and the light at
  which a signed mode leaves the colour unchanged. NULL takes the
  preset's when `mode` is the preset's mode, else 45.

- shadow_colour, highlight_colour:

  Tone colours of `"tone"`.

- linear:

  Whether `"multiply"` and `"tint"` work in linear light (`TRUE`) or on
  encoded sRGB values (`FALSE`). NULL takes the preset's.

- material:

  A
  [`material_3d()`](https://gillescolling.com/ggplot3d/reference/material_3d.md)
  for the shaded layers that set none, or NULL for the preset's (plain
  Lambert reflection when the preset has none).

- viewer:

  NULL, or the distance of the eye that the specular highlight is
  computed for, in radii of the sphere around the panel box, measured
  from the camera target along the view axis (OpenGL's local viewer; rgl
  places it at `1 / sin(fov / 2)`). A near eye draws a highlight out
  along a surface. NULL takes the preset's, else the camera's eye
  (infinitely far under orthographic projection).

- preset:

  Starting lighting: `"default"`, `"contrast"`, `"shade"`,
  `"headlight"`, `"soft"` or `"studio"`.

## Value

A lighting configuration for
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).
The path tracer takes every light. The intrinsic raymarch backend takes
the first directional light.

## Details

`preset` picks a starting lighting; every argument given explicitly
replaces the preset's value, and lights given in `...` replace its
lights. Except for the default, each preset reproduces the surface
lighting of a widely used 3D package, named below.

- `"default"`: one
  [`light_directional_3d()`](https://gillescolling.com/ggplot3d/reference/light_directional_3d.md),
  ambient 0.35, `mode = "lightness"`, `range = c(-0.45, 0.4)`,
  `neutral = 60`.

- `"contrast"`: the full range from black to white with a light from the
  side: a light along `c(-0.5, 0, 1)` in the data axes,
  `mode = "value"`, `range = c(-1, 1)`, `neutral = 90` (ggcube's
  `light()` in its `"hsv"` mode at `contrast = 1`).

- `"shade"`: darkening only, by up to 70 percent, from a low light: the
  colour scaled by `0.65 + 0.35 * cos` with the light at azimuth 225 and
  altitude 19.4712 degrees in the data axes, i.e. `mode = "value"`,
  `range = c(-0.7, 0)`, `neutral = 0` (matplotlib's `plot_surface()`).

- `"headlight"`: a white directional light at the eye with no ambient
  reflection and a white highlight (specular 1, shininess 50) lit at the
  vertices (`lighting = "vertex"`) on encoded values, and the eye placed
  for the highlight at `viewer = 1 / sin(15 degrees)`, the distance of a
  30-degree field of view (a new rgl scene, as desktop rgl's
  fixed-function OpenGL draws it).

- `"soft"`: a strong ambient term (0.8) with diffuse 0.8 from a light
  straight above the viewer and a faint broad highlight (specular 0.05,
  Blinn exponent 6), on encoded values (plotly's surface lighting
  defaults).

- `"studio"`: ambient `rgb(0.4, 0.2, 0.2)` and four coloured directional
  lights fixed to the image, shining toward its centre from the points
  `(2, 0, 2)`, `(2, 2, 3)`, `(0, 2, 2)` and `(0, 0, 2)` in image-scaled
  coordinates, on encoded values (Mathematica's
  `Lighting -> Automatic`). It tints the surface and suits light fills.

`mode` sets how the light changes a face colour. `"multiply"` scales the
colour by the ambient and diffuse light and `"tint"` scales its distance
from white; both only darken, so a shaded fill no longer matches its
legend key, and with the default light, which comes from near the
viewer, the faces that face the camera all receive almost the same
light. The signed modes darken faces turned from the light and lighten
faces turned toward it, which keeps the colour readable against its key
(Szafir et al. 2016; Kovesi 2015) and separates the faces that face the
camera; the default is the signed mode `"lightness"`. Each light gives a
level in \[-1, 1\] from the angle between the normal and the light: 0 at
`neutral` degrees, 1 facing the light and -1 facing away, linear in the
cosine on each side (ggcube's `ramp_smooth()`). The levels are averaged
over the lights, weighted by their luminance, and scaled by the
material's diffuse strength. A level of 0 leaves the colour unchanged.

- `"lightness"` moves the OKLab lightness (Ottosson 2020) toward black
  by up to the fraction `abs(range[1])` in shadow and toward white by up
  to `range[2]` in light, and keeps the hue; where the moved colour
  leaves the sRGB gamut, its chroma is reduced until it fits (ggcube's
  `tone_lightness()`).

- `"tone"` moves the colour in OKLab toward `shadow_colour` or
  `highlight_colour`, up to the fractions `abs(range)`.

- `"overlay"` and `"soft"` are matplotlib's `LightSource` blend modes on
  the encoded colour, with an intensity of
  `0.5 + level * abs(range) / 2`.

- `"value"` scales the HSV value of the encoded colour toward 0 by up to
  the fraction `abs(range[1])` in shadow and moves it toward 1 by up to
  `range[2]` in light, keeping hue and saturation (ggcube's `"hsv"`
  mode).

`multiply` and `tint` work in linear light. With `linear = FALSE` they
work on the encoded sRGB values instead, with the light colours and
ambient taken as encoded values too and the light reaching a face capped
at 1, as the fixed-function OpenGL pipeline, rgl, plotly and Mathematica
do; the result is darker in the half shadow.

Specular highlights of
[`material_3d()`](https://gillescolling.com/ggplot3d/reference/material_3d.md)
are added on top in every mode. Lightness-monotone palettes such as
viridis lose the most under shading; isoluminant or diverging palettes
combine best with relief shading.
[`guide_colourbar_3d()`](https://gillescolling.com/ggplot3d/reference/guide_colourbar_3d.md),
[`guide_coloursteps_3d()`](https://gillescolling.com/ggplot3d/reference/guide_colourbar_3d.md),
[`guide_bins_3d()`](https://gillescolling.com/ggplot3d/reference/guide_colourbar_3d.md)
and
[`guide_legend_3d()`](https://gillescolling.com/ggplot3d/reference/guide_colourbar_3d.md)
show the shaded colours of a key.

## References

Szafir, Sarikaya, Gleicher (2016). Lightness constancy in surface
visualization. IEEE TVCG 22, 2107-2121.
[doi:10.1109/TVCG.2015.2500240](https://doi.org/10.1109/TVCG.2015.2500240)

Kovesi (2015). Good colour maps: how to design them.
<https://arxiv.org/abs/1509.03700>

Ottosson (2020). A perceptual color space for image processing.
<https://bottosson.github.io/posts/oklab/>

## Examples

``` r
light_3d(light_head_3d(), light_hemisphere_3d("lightblue", "brown"), ambient = 0.1)
d <- expand.grid(x = seq(-2, 2, length.out = 25), y = seq(-2, 2, length.out = 25))
d$z <- with(d, exp(-(x^2 + y^2)))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) +
  geom_surface_3d(shading = "smooth", colour = NA) +
  ggplot2::scale_fill_distiller(palette = "Spectral") +
  coord_3d(light = light_3d(light_directional_3d(), mode = "lightness"))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "grey85", shading = "smooth", colour = NA) +
  coord_3d(light = light_3d(preset = "studio"))
```
