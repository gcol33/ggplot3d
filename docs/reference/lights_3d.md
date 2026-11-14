# Combine lights for a 3D scene

`mode` sets how the light changes a face colour. `"multiply"` scales the
colour by the ambient and diffuse light and `"tint"` scales its distance
from white; both darken every channel, so a shaded fill no longer
matches its legend key. The signed modes keep the colour readable
against its key (Szafir et al. 2016; Kovesi 2015). Each light gives a
level in \[-1, 1\] from the angle between the normal and the light: 0 at
`neutral` degrees, 1 facing the light and -1 facing away, linear in the
cosine on each side (ggcube's `ramp_smooth()`). The levels are averaged
over the lights, weighted by their luminance, and scaled by the
material's diffuse strength. A level of 0 leaves the colour unchanged.

## Usage

``` r
lights_3d(
  ...,
  ambient = 0.35,
  mode = "multiply",
  range = NULL,
  neutral = 45,
  shadow = "#1d2b53",
  highlight = "#fff1d6"
)
```

## Arguments

- ...:

  Lights made with
  [`light_directional()`](https://gillescolling.com/ggplot3d/reference/light_directional.md),
  [`light_point()`](https://gillescolling.com/ggplot3d/reference/light_directional.md),
  [`light_head()`](https://gillescolling.com/ggplot3d/reference/light_directional.md)
  or
  [`light_hemisphere()`](https://gillescolling.com/ggplot3d/reference/light_directional.md).

- ambient:

  Nonnegative strength of the uniform ambient light.

- mode:

  Colour operation: `"multiply"`, `"tint"`, `"lightness"`, `"tone"`,
  `"overlay"` or `"soft"`.

- range:

  Moves at full shadow and full highlight, `c(shadow, highlight)` with
  `-1 <= shadow <= 0 <= highlight <= 1`: fractions of the way to black
  and white in OKLab lightness for `"lightness"`, to the tone colours
  for `"tone"`, blend strengths for `"overlay"` and `"soft"`. NULL takes
  `c(-0.45, 0.3)` for `"lightness"` and `c(-0.6, 0.6)` otherwise.

- neutral:

  Angle in degrees, in \[0, 180), between the normal and the light at
  which a signed mode leaves the colour unchanged.

- shadow, highlight:

  Tone colours of `"tone"`.

## Value

A lighting configuration for
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).
The backends that take one directional light (the path tracer and the
raymarch backend) use the first directional light.

## Details

- `"lightness"` moves the OKLab lightness (Ottosson 2020) toward black
  by up to the fraction `abs(range[1])` in shadow and toward white by up
  to `range[2]` in light, and keeps the hue; where the moved colour
  leaves the sRGB gamut, its chroma is reduced until it fits (ggcube's
  `tone_lightness()`).

- `"tone"` moves the colour in OKLab toward `shadow` or `highlight`, up
  to the fractions `abs(range)`.

- `"overlay"` and `"soft"` are matplotlib's `LightSource` blend modes on
  the encoded colour, with an intensity of
  `0.5 + level * abs(range) / 2`.

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
lights_3d(light_head(), light_hemisphere("lightblue", "brown"), ambient = 0.1)
d <- expand.grid(x = seq(-2, 2, length.out = 25), y = seq(-2, 2, length.out = 25))
d$z <- with(d, exp(-(x^2 + y^2)))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) +
  geom_surface_3d(shading = "smooth", colour = NA) +
  ggplot2::scale_fill_distiller(palette = "Spectral") +
  coord_3d(light = lights_3d(light_directional(), mode = "lightness"))
```
