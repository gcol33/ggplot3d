# Depth cues: fog, line width and eye-dome lighting

Configures the `depth_cue` argument of
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).
Fog moves the colours of the plot layers toward a fog colour with their
depth from the camera, in linear light, so far data recede. The same
weight can thin strokes with depth, as `scale_depth` does in ggcube.
Eye-dome lighting (Boucheny 2009) darkens rasterised pixels that lie
behind their neighbours, which outlines silhouettes and depth steps from
the depth buffer alone.

## Usage

``` r
depth_cue_3d(
  fog = NULL,
  near = 0,
  far = 1,
  amount = 0.6,
  linewidth = FALSE,
  edl = FALSE,
  ao = FALSE,
  ao_radius = 0.25,
  ao_samples = 64L
)
```

## Arguments

- fog:

  Fog colour. `NULL` uses the fill of the theme element
  `panel.background` (white when it is blank); `NA` leaves colours
  unchanged.

- near, far:

  Box depth fractions at which the fog weight starts to grow from 0 and
  reaches 1; `near < far`.

- amount:

  Share of the fog colour at full weight, from 0 to 1.

- linewidth:

  `FALSE`, `TRUE` (0.5), or a number from 0 to 1: the share by which
  strokes thin at full weight.

- edl:

  `FALSE`, `TRUE` (1), or a nonnegative eye-dome lighting strength.

- ao:

  `FALSE`, `TRUE` (1), or ambient occlusion strength from 0 to 1. Acts
  on shaded mesh faces with enabled lighting.

- ao_radius:

  Positive ray distance in aspect-scaled panel units.

- ao_samples:

  Positive whole number of deterministic hemisphere rays.

## Value

A depth cue configuration for
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md).

## Details

The fog weight of a primitive is \$\$w = \min(1, \max(0, (s - near) /
(far - near))),\$\$ where \\s\\ is the camera depth of the primitive's
centre as a fraction of the depth extent of the panel box: 0 at the box
corner nearest the camera, 1 at the farthest. Geometry fill and stroke
colours move toward the fog colour by `amount * w`, and with `linewidth`
a stroke's width is multiplied by `1 - linewidth * w`. Panel furniture
keeps its colours. The vector, raster and hybrid backends evaluate the
weight once per primitive, so tessellated surfaces fade smoothly;
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
evaluates the same formula per pixel.

Ambient occlusion samples the hemisphere above each mesh vertex against
a mesh bounding-volume hierarchy, following Pharr and Green (GPU Gems,
chapter 17). Its accessibility estimates mean hemisphere transmission
with deterministic rays within `ao_radius`, including face and texture
opacity. Shaded face colours are multiplied in the shared kernel by
`1 - ao * (1 - accessibility)`. The vector, raster and hybrid backends
interpolate these values over faces. Both sides of a surface have their
own accessibility. Text and label boxes keep their colours and draw
last. The interactive viewer uses `edl` for occlusion shading.

Eye-dome lighting acts on the raster backend, the rasterised layers of
the hybrid backend, and
[`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md).
A pixel darkens by `exp(-40 * edl * r)`, where `r` averages over its
eight neighbours how far each lies in front of it in log2 of the depth
normalised to the camera's near and far planes, as for
`geom_point_3d(render = "splat")`.

## References

Pharr and Green (2004). Ambient Occlusion. GPU Gems, chapter 17.
<https://developer.nvidia.com/gpugems/gpugems/part-iii-materials/chapter-17-ambient-occlusion>

Boucheny, C. (2009). Visualisation scientifique de grands volumes de
donnees: pour une approche perceptive. PhD thesis, Universite Joseph
Fourier, Grenoble.

Kitware. Eye-Dome Lighting: a non-photorealistic shading technique.
<https://www.kitware.com/eye-dome-lighting-a-non-photorealistic-shading-technique/>

## Examples

``` r
d <- expand.grid(x = 1:6, y = 1:6, z = 1:6)
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 2)
p + coord_3d(depth_cue = depth_cue_3d())
p + coord_3d(depth_cue = depth_cue_3d(fog = "grey90", amount = 0.8, linewidth = TRUE))
v <- expand.grid(x = seq(-1, 1, length.out = 30), y = seq(-1, 1, length.out = 30))
v$z <- sin(3 * v$x) * cos(3 * v$y)
ggplot2::ggplot(v, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "grey80", colour = NA, shade = FALSE) +
  coord_3d(backend = backend_3d("raster", dpi = 72), light = NULL,
    depth_cue = depth_cue_3d(fog = NA, edl = 2))
a <- expand.grid(x = seq(0, 1, length.out = 7), y = seq(0, 1, length.out = 7))
a$z <- with(a, abs(x - 0.5) + abs(y - 0.5))
ggplot2::ggplot(a, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "grey70", colour = NA) +
  coord_3d(depth_cue = depth_cue_3d(fog = NA, ao = TRUE, ao_radius = 0.5))
```
