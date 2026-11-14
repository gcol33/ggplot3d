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
  rasterise = "marked"
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

  Path-tracing samples per pixel added by each render.

- bounces:

  Maximum diffuse surface interactions per path.

- seed:

  Nonnegative integer seed for pixel and sample indexed randomness.

- aperture:

  Thin-lens radius in aspect-scaled model units; zero is a pinhole.

- focus:

  Focal-plane distance along the camera forward axis, in model units.
  NULL focuses on the camera target.

- light_radius:

  Angular radius of the distant light in degrees.

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

## Value

A backend configuration for coord_3d().

## Details

The raster backend renders 64-pixel tiles in parallel. The number of
worker threads is the option `ggplot3d.threads`; the default `0` uses
one thread per hardware thread, at most two when `_R_CHECK_LIMIT_CORES_`
is set. The image does not depend on the thread count.

The native path tracer uses diffuse materials, a bounding-volume
hierarchy, cosine-weighted reflection sampling and Russian roulette. The
ambient light is a uniform environment; the diffuse light is a sampled
distant disk. Points and paths use the solid conversion of
[`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md),
and SDF layers use the shared sphere tracer. Furniture and text remain
vector graphics.

The raymarch backend traces geodesics from an observer inside the
geometry. Use
[`geom_thurston_3d()`](https://gillescolling.com/ggplot3d/reference/geom_thurston_3d.md)
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

## Examples

``` r
backend_3d("vector")
backend_3d("raster", dpi = 144, antialias = 2)
backend_3d("raster", blend = "srgb")
backend_3d("pathtrace", samples = 8, seed = 1)
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
