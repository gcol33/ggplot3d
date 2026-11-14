# Lighting and surface styles

A lit surface shows its shape through brightness: faces turned toward
the light are brighter than faces turned away. When the fill also
carries data, the two compete for the same channel. This article goes
through the ways ggplot3d lets shading and colour share a figure: colour
modes that keep the fill readable, guides that show what the light does
to each colour, illustrative surface styles, line drawings, and lighting
compared across panels.

``` r

library(ggplot3d)
library(ggplot2)
theme_set(theme_minimal(base_size = 17) + theme(
  plot.background = element_rect(fill = "transparent", colour = NA),
  panel.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA),
  legend.key = element_rect(fill = "transparent", colour = NA),
  legend.box.background = element_rect(fill = "transparent", colour = NA)))
d <- expand.grid(x = seq(-2, 2, length.out = 36), y = seq(-2, 2, length.out = 36))
d$z <- with(d, exp(-(x^2 + y^2)) + 0.5 * exp(-((x - 1)^2 + (y + 1)^2) * 2))
hill <- ggplot(d, aes(x, y, z = z, fill = z)) +
  geom_surface_3d(shading = "smooth", colour = NA)
```

## Colour that survives shading

By default the light multiplies the fill: a face at a grazing angle is a
darker version of its colour, and a dark yellow no longer looks like the
yellow in the legend. `lights_3d(mode = "lightness")` moves only the
OKLab lightness of the fill and keeps its hue, so the colour stays
recognisable against its key. A face at the `neutral` angle to the light
keeps its colour exactly.

``` r

hill + scale_fill_distiller(palette = "Spectral",
    guide = guide_colourbar_3d(along = NULL, shading.range = TRUE)) +
  coord_3d(light = lights_3d(light_directional(), mode = "lightness"))
```

![3D plot. Layers: Surface (x = x, y = y, z = z, fill = z). Camera:
azimuth -60 degrees, elevation 30 degrees, roll 0 degrees; orthographic
projection.](lighting_files/figure-html/lightness-1.svg)

The colourbar now shows each value from shadow (left) to full light
(right), sampled from the same shading kernel as the faces. A palette
that changes lightness with value, such as viridis, loses the most under
shading, since shading changes lightness too; a diverging or isoluminant
palette leaves lightness free for the shape.

`"tone"` moves the colour toward a shadow and a highlight colour
instead, and `"overlay"` and `"soft"` are the blend modes of
matplotlib’s `LightSource`.

``` r

hill + scale_fill_viridis_b(guide = guide_coloursteps_3d()) +
  coord_3d(light = lights_3d(light_directional(), mode = "tone",
    shadow = "#22305a", highlight = "#fff3d0"))
```

![3D plot. Layers: Surface (x = x, y = y, z = z, fill = z). Camera:
azimuth -60 degrees, elevation 30 degrees, roll 0 degrees; orthographic
projection.](lighting_files/figure-html/tone-1.svg)

## Illustrative styles

`material_3d(style = )` changes how a face responds to the light. Toon
shading quantises the response into bands, Gooch shading (Gooch et al.
1998) turns brightness into a cool-to-warm hue shift that keeps dark
regions readable, and normal colouring encodes the orientation of each
face as a colour.

``` r

plain <- ggplot(d, aes(x, y, z = z)) +
  coord_3d(panes = character(), axes = character(), box = "none")
toon <- plain + geom_surface_3d(fill = "tomato", colour = NA,
  material = material_3d(style = "toon", bands = 4)) + ggtitle("toon")
gooch <- plain + geom_surface_3d(fill = "grey60", colour = NA, shading = "smooth",
  material = material_3d(style = "gooch")) + ggtitle("gooch")
normals <- plain + geom_surface_3d(colour = NA, shading = "smooth",
  material = material_3d(style = "normals")) + ggtitle("normals")
patchwork::wrap_plots(toon, gooch, normals, nrow = 1)
```

![3D plot. Layers: Surface (x = x, y = y, z = z). Camera: azimuth -60
degrees, elevation 30 degrees, roll 0 degrees; orthographic projection.
3D plot. Layers: Surface (x = x, y = y, z = z). Camera: azimuth -60
degrees, elevation 30 degrees, roll 0 degrees; orthographic projection.
3D plot. Layers: Surface (x = x, y = y, z = z). Camera: azimuth -60
degrees, elevation 30 degrees, roll 0 degrees; orthographic
projection.](lighting_files/figure-html/styles-1.svg)

## Line drawings

[`hatch_3d()`](https://gillescolling.com/ggplot3d/reference/hatch_3d.md)
draws the tone as hatching. The strokes are cut from the surface by
parallel planes, so they follow it in depth and are hidden by nearer
parts like any other line; darker faces keep more of them. With a white
fill, feature lines from
[`outline_3d()`](https://gillescolling.com/ggplot3d/reference/outline_3d.md)
and the plotter backend, which draws strokes only, the figure is a line
drawing that an SVG device writes for a pen plotter.

``` r

ggplot(d, aes(x, y, z = z)) +
  geom_surface_3d(fill = "white", colour = NA, hatch = hatch_3d(angle = 0, spacing = 0.02),
    outline = outline_3d(linewidth = 0.4)) +
  coord_3d(backend = backend_3d("plotter"), panes = character(), box = "none")
```

![3D plot. Layers: Surface (x = x, y = y, z = z). Camera: azimuth -60
degrees, elevation 30 degrees, roll 0 degrees; orthographic
projection.](lighting_files/figure-html/hatch-1.svg)
