# Select a 3D rendering backend

Select a 3D rendering backend

## Usage

``` r
backend_3d(
  type = "vector",
  dpi = 144,
  antialias = 2L,
  blend = "linear",
  samples = 64L,
  bounces = 8L,
  seed = 1L,
  aperture = 0,
  focus = NULL,
  light_radius = 5,
  progressive = FALSE,
  batch_size = 8L,
  geometry = "e3",
  observer = c(0, 0, 0),
  facing = diag(3),
  fov = 70,
  max_distance = 10,
  tolerance = 1e-05,
  max_steps = 2048L,
  quotient = NULL,
  light_size = 0.1,
  rasterise = "marked",
  noise_threshold = 0,
  min_samples = 16L,
  denoise = FALSE,
  exposure = 0,
  tone = "clip"
)
```

## Arguments

- type:

  Backend: `"vector"`, `"raster"`, `"hybrid"`, `"pathtrace"`,
  `"raymarch"` (an intrinsic view in a Thurston geometry) or
  `"plotter"`. The plotter backend is the vector backend drawing strokes
  only: faces and analytic primitives still hide the strokes behind them
  but are not painted, and consecutive visible pieces of one stroke are
  drawn as one path, so an SVG device
  (`ggplot2::ggsave(device = "svg")`) writes a drawing for a pen
  plotter. Feature lines
  ([`outline_3d()`](https://gillescolling.com/ggplot3d/reference/outline_3d.md))
  and model-space hatching
  ([`hatch_3d()`](https://gillescolling.com/ggplot3d/reference/hatch_3d.md))
  make line-only figures of surfaces. Stroke centrelines are clipped to
  visible intervals, joined within each source polyline, and ordered
  within each layer and colour by the nearest endpoint, reversing paths
  to shorten pen travel (as in vpype's `linesort`). Consecutive points
  within 0.01 mm or one tenth of a device pixel are combined. Text and
  labels are drawn last. A hybrid panel rasterises the layers created
  with `raster = TRUE` (an argument of every `geom_*_3d()` layer and of
  [`layer_3d()`](https://gillescolling.com/ggplot3d/reference/layer_3d.md))
  into one image at `dpi` and draws the other layers, axes, panes and
  text as vector graphics. Vector geometry behind the image's opaque
  coverage is clipped at its pixels, so lines on or behind a rasterised
  surface stay exact where they are seen.

- dpi:

  Physical raster resolution.

- antialias:

  Positive integer supersampling factor.

- blend:

  Colour space in which the raster backend composites translucent paint
  and averages antialiasing samples: `"linear"` light, encoded to sRGB
  after averaging, or `"srgb"`, which matches the compositing of vector
  graphics devices.

- samples:

  Maximum path-tracing samples per pixel added by each render.

- bounces:

  Maximum surface or medium scattering interactions per path.

- seed:

  Nonnegative integer seed for pixel and sample indexed randomness.

- aperture:

  Thin-lens radius in aspect-scaled model units; zero is a pinhole.

- focus:

  Focal-plane distance along the camera forward axis, in model units.
  NULL focuses on the camera target.

- light_radius:

  Angular radius of each directional or head light in degrees.

- progressive:

  Accumulate samples across redraws of the same scene. Changes to
  geometry, camera, lighting or image dimensions reset accumulation.

- batch_size:

  Samples per native batch; accumulation is independent of batch size
  and worker count. R can interrupt between batches.

- geometry:

  Intrinsic geometry for the raymarch backend; see
  [`geodesic_3d()`](https://gillescolling.com/ggplot3d/reference/geodesic_3d.md).

- observer:

  Observer coordinates in that geometry's chart.

- facing:

  Orthonormal 3 by 3 matrix whose columns are the camera's right, up and
  forward tangent vectors in the observer's orthonormal frame.

- fov:

  Vertical intrinsic field of view in degrees.

- max_distance:

  Maximum intrinsic ray arc length.

- tolerance:

  Surface residual and quotient-boundary tolerance.

- max_steps:

  Maximum steps per intrinsic ray.

- quotient:

  List of paired
  [`thurston_portal_3d()`](https://gillescolling.com/ggplot3d/reference/thurston_portal_3d.md)
  domain faces.

- light_size:

  Positive point-light softening radius for intrinsic views.

- rasterise:

  Layers a hybrid panel draws into its image: `"marked"`, the layers
  created with `raster = TRUE`; `"surfaces"`, which adds every
  depth-tested layer that draws faces or analytic quadrics; or `"all"`,
  which adds every depth-tested layer with faces, paths or points. Text
  stays vector graphics, placed by the image's visibility.

- noise_threshold:

  Nonnegative relative standard-error target. Zero disables adaptive
  sampling; positive values stop converged pixels after `min_samples`.
  Coverage is tested with absolute standard error. Dark channels use a
  linear-radiance floor of 0.001. Adaptive stopping is an estimate and
  can introduce stopping bias.

- min_samples:

  Positive minimum sample count before adaptive stopping. At least two
  samples per independent half buffer are required.

- denoise:

  Apply native nonlinearly weighted first-order regression using
  independent colour, albedo, normal and depth half buffers.

- exposure:

  Exposure adjustment in stops, applied in linear light.

- tone:

  `"clip"`, `"aces"` (Narkowicz fit), or `"agx"` (the Rec.2020 AgX
  transform with a polynomial contrast approximation).

## Value

A `ggplot3d_backend` configuration for
[`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
and
[`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md),
validated at construction and use.

## Details

The raster backend renders 16-pixel tiles in parallel. The number of
worker threads is the option `ggplot3d.threads`; the default `0` uses
one thread per hardware thread, at most two when `_R_CHECK_LIMIT_CORES_`
is set. The image does not depend on the thread count.

The native path tracer uses
[`material_3d()`](https://gillescolling.com/ggplot3d/reference/material_3d.md)
BSDFs, a bounding-volume hierarchy, Owen-scrambled Sobol samples,
next-event light sampling with power-heuristic multiple importance
sampling and Russian roulette. Ambient light is a uniform environment;
directional lights are sampled distant disks. Point, head, hemisphere,
image environments and emissive triangles contribute. Directional and
point intensities retain the Lambert convention of the other backends
(incident irradiance is pi times intensity). Absorption volumes use
delta tracking for scattering and ratio tracking for shadow
transmittance, with conservative per-cell extinction majorants.
Isovolumes intersect the trilinear field as a surface. MIP is
unavailable. Points and paths use the solid conversion of
[`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md),
and SDF layers use the shared sphere tracer. Furniture and text remain
vector graphics.

The raymarch backend traces geodesics from an observer inside the
geometry. Use
[`annotation_thurston_3d()`](https://gillescolling.com/ggplot3d/reference/annotation_thurston_3d.md)
for balls, cylinders and half-spaces, or traced SDF layers for
chart-defined CSG. It draws a full-panel perspective image; Euclidean
plot axes and exterior-camera settings have no intrinsic meaning.
Lighting is an isotropic point light at the observer, with the ambient
and diffuse strengths from
[`light_3d()`](https://gillescolling.com/ggplot3d/reference/light_3d.md).
Irradiance divides by the area density of geodesic spheres plus
`light_size^2`. Each visible geodesic supplies its own light path,
including paths through quotient faces. Intrinsic surfaces are opaque.
See
[`geodesic_3d()`](https://gillescolling.com/ggplot3d/reference/geodesic_3d.md)
for charts and numerical accuracy details.

## References

Pharr, Jakob and Humphreys (2023). Physically Based Rendering, fourth
edition.
<https://pbr-book.org/4ed/Light_Transport_I_Surface_Reflection/Path_Tracing>

Veach (1997). Robust Monte Carlo methods for light transport simulation.
Chapter 9. <https://graphics.stanford.edu/papers/veach_thesis/> Veach
and Guibas (1995). Optimally combining sampling techniques for Monte
Carlo rendering.
[doi:10.1145/218380.218498](https://doi.org/10.1145/218380.218498)

Burley (2020). Practical hash-based Owen scrambling. JCGT 9(4).
<https://jcgt.org/published/0009/04/01/>

Bitterli et al. (2016). Nonlinearly weighted first-order regression for
denoising Monte Carlo renderings.
[doi:10.1111/cgf.12954](https://doi.org/10.1111/cgf.12954)

Novak et al. (2014). Residual ratio tracking for estimating attenuation
in participating media.
[doi:10.1145/2661229.2661292](https://doi.org/10.1145/2661229.2661292)

Narkowicz (2016). ACES filmic tone mapping curve.
<https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/>

Three.js AgX tone-mapping implementation, adapted from Filament.
<https://github.com/mrdoob/three.js/blob/dev/src/renderers/shaders/ShaderChunk/tonemapping_pars_fragment.glsl.js>

## Examples

``` r
backend_3d("vector")
backend_3d("raster", dpi = 144, antialias = 2)
backend_3d("raster", blend = "srgb")
backend_3d("pathtrace", samples = 8, seed = 1)
backend_3d("pathtrace", samples = 16, noise_threshold = 0.05, denoise = TRUE, tone = "agx")
d <- expand.grid(x = seq(-1, 1, length.out = 40), y = seq(-1, 1, length.out = 40))
d$z <- sin(pi * d$x) * cos(pi * d$y)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "grey80", colour = NA, raster = TRUE) +
  geom_contour_3d(colour = "black") +
  coord_3d(backend = backend_3d("hybrid", dpi = 150))
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  geom_surface_3d(fill = "grey80", colour = NA) +
  coord_3d(backend = backend_3d("hybrid", dpi = 150, rasterise = "surfaces"))
```
