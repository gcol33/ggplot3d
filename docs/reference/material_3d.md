# Surface materials

A material sets how strongly a face reflects the ambient, diffuse and
specular light of
[`lights_3d()`](https://gillescolling.com/ggplot3d/reference/lights_3d.md).
The presets follow MATLAB's `material`: `"matte"` (MATLAB `dull`),
`"shiny"` and `"metal"`, with the ambient and diffuse strengths taken
relative to `dull` so that `"matte"` reproduces plain Lambert shading.
The specular highlight is Blinn's (1977) halfway term
`specular * max(0, n . h)^shininess`; its colour moves from white to the
face colour as `specular_colour` goes from 1 to 0 (MATLAB's
`SpecularColorReflectance`). `back` colours the back side of faces, the
side a face's normal points away from; NULL lights both sides alike.

## Usage

``` r
material_3d(
  type = "matte",
  ambient = NULL,
  diffuse = NULL,
  specular = NULL,
  shininess = NULL,
  specular_colour = NULL,
  back = NULL,
  style = "standard",
  bands = 3L,
  breaks = NULL,
  specular_steps = NULL,
  cool = "#0000AA",
  warm = "#AAAA00",
  alpha = 0.2,
  beta = 0.6,
  normal_space = "model"
)
```

## Arguments

- type:

  Preset: `"matte"`, `"shiny"` or `"metal"`.

- ambient, diffuse, specular:

  Nonnegative strengths; NULL keeps the preset.

- shininess:

  Positive specular exponent; NULL keeps the preset.

- specular_colour:

  Specular colour reflectance in \[0, 1\]; NULL keeps the preset.

- back:

  NULL or one colour for the back side of faces.

- style:

  `"standard"`, `"toon"`, `"gooch"` or `"normals"`.

- bands:

  Number of toon bands, at least two.

- breaks:

  NULL or increasing toon band breaks in degrees in (0, 180).

- specular_steps:

  Whether the highlight is a flat spot; NULL takes `TRUE` for toon and
  `FALSE` otherwise.

- cool, warm:

  Gooch colours.

- alpha, beta:

  Gooch weights of the fill in the cool and warm colours.

- normal_space:

  Frame of the normal colours: `"model"`, `"camera"` or `"light"`.

## Value

A material for the `material` argument of the surface layers.

## Details

`style` selects an illustrative response, computed by the same shading
kernel in the vector and raster backends and the viewer:

- `"toon"` quantises each light's diffuse response into `bands` equal
  steps of the cosine, or at the angles `breaks` (degrees from the
  light, increasing, at most seven; a normal exactly at a break, within
  1e-6 in cosine, falls in the darker band), as ggcube's `ramp_steps()`.
  With the signed modes of
  [`lights_3d()`](https://gillescolling.com/ggplot3d/reference/lights_3d.md)
  the steps divide the signed level instead. `specular_steps = TRUE`
  (the default for toon) turns the highlight into a flat spot where
  Blinn's term reaches one half.

- `"gooch"` is the cool-to-warm shading of Gooch et al. (1998): the
  colour blends from `cool + alpha * fill` to `warm + beta * fill` as
  `(1 + n . l) / 2` goes from 0 to 1, averaged over the lights; the
  defaults are the paper's `k_blue = (0, 0, 0.4)` and
  `k_yellow = (0.4, 0.4, 0)` in linear light with `alpha = 0.2`,
  `beta = 0.6`.

- `"normals"` replaces the fill by the unit normal encoded as
  `(n + 1) / 2` in sRGB (three.js `MeshNormalMaterial`), in the panel's
  axes (`normal_space = "model"`), the camera's right, up and toward
  axes (`"camera"`), or a frame whose third axis points at the first
  contributing light (`"light"`). It takes no light and no specular
  term, and the layer's fill legend is dropped.

Styles need a light: with `coord_3d(light = NULL)` faces keep their
fill. In the path tracer, illustrative surfaces return the shared
kernel's colour at the hit point and terminate the path, including
indirect hits.

## References

Gooch, Gooch, Shirley, Cohen (1998). A non-photorealistic lighting model
for automatic technical illustration. SIGGRAPH '98, 447-452.
[doi:10.1145/280814.280950](https://doi.org/10.1145/280814.280950)

## Examples

``` r
material_3d("shiny")
material_3d("metal", back = "grey40")
material_3d(style = "toon", breaks = c(40, 75))
d <- expand.grid(x = seq(-1, 1, length.out = 12), y = seq(-1, 1, length.out = 12))
d$z <- with(d, x^2 - y^2)
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
p + geom_surface_3d(shading = "smooth", material = material_3d("shiny")) + coord_3d()
p + geom_surface_3d(fill = "grey60", colour = NA, material = material_3d(style = "gooch")) + coord_3d()
p + geom_surface_3d(fill = "tomato", colour = NA, material = material_3d(style = "toon", bands = 4)) + coord_3d()
p + geom_surface_3d(colour = NA, shading = "smooth", material = material_3d(style = "normals")) + coord_3d()
```
