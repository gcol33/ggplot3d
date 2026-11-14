# Texture edge continuity, 2026-11-14

I reproduced the `layers.Rmd` drape chunk at 110 and 300 dpi, using the
original 6.5 by 4.5 inch figure and its 1,120 coarse volcano triangles.
`raster3d = TRUE` calls `knit_static_3d()` and selects the hybrid backend
with `rasterise = "surfaces"`. Its surface uses the C++ raster sampler.

The shared UV interpolation was already continuous. Under the orthographic
camera, affine interpolation within each triangle is exact. Under perspective,
the sampler normalises the barycentric weights divided by corner depth.
The continuity target is the intersection of a checker boundary with each
shared triangle edge. The projected boundary follows the piecewise planar
surface.

I found two sampling problems:

* `interpolate = FALSE` selected nearest sampling only under magnification.
  Minification selected bilinear/trilinear mipmaps. Their footprint depends
  on each triangle's UV derivatives, so adjoining triangles could apply
  different filters. In the drape, this changed 16 interior pixels at 110 dpi.
  The 300 dpi raster matched the texel-clipped geometry exactly.
* Unlit vector textures used device raster groups transformed and resampled
  per triangle. The svglite affine transforms also lost precision. Lit runs
  used cropped, device-resolution images and compositing groups. Device
  rasterisation of these images displaced checker edges, especially on PDF
  and Cairo output.

Static backends now use the same texture and lighting kernel. A vector face
run embeds one RGBA image on the full panel grid. Opaque runs use four by four
samples per pixel and at least 300 dpi. Translucent runs retain sampling at
device pixel centres so separate depth-order runs retain disjoint coverage.
Nearest sampling always selects a texel from the original image. Text and
labels retain the final drawing pass.

The sampling rules follow the [OpenGL sampler specification](https://wikis.khronos.org/opengl/Sampler_Object)
and the perspective-correct interpolation contract in the
[OpenGL Shading Language specification](https://registry.khronos.org/OpenGL/specs/gl/GLSLangSpec.4.60.pdf).

## Measurement

I clipped each source triangle against the checker texels in UV space and
projected the resulting coloured polygons. This independent geometric
reference contains 1,504 polygons. It is rasterised with eight by eight
samples per pixel. Each output device renders both the reference and the
texture before comparison, accounting for its pixel placement.

The measurement extracts perpendicular checker-edge profiles two pixels
from triangle-edge crossings. It retains isolated, monotone reference
transitions and interpolates their half-coverage positions. The tolerance
is 0.5 device pixels. Neutral lighting exercises the lit path using the
same checker colours. The raster/hybrid plot comparison also checks their
complete rendered PNGs for equality.

| Device | dpi | Original unlit maximum, px | Original lit maximum, px | New maximum, px | Profiles |
|---|---:|---:|---:|---:|---:|
| ragg | 110 | 1.148 | 0.448 | 0.065 | 258 |
| svglite | 110 | missing crossings | 0.524 | 0.062 | 244 |
| PDF | 110 | missing crossings | 2.004 | 0.060 | 169 |
| Cairo | 110 | missing crossings | 2.004 | 0.060 | 169 |
| ragg | 300 | 1.018 | 0.496 | 0.202 | 520 |
| svglite | 300 | missing crossings | 0.496 | 0.202 | 520 |
| PDF | 300 | missing crossings | 1.258 | 0.165 | 293 |
| Cairo | 300 | missing crossings | 0.747 | 0.165 | 293 |

The original unlit path had 6, 23 and 23 missing crossings at 110 dpi in
svglite, PDF and Cairo; at 300 dpi it had 238, 43 and 48. A missing crossing
has no half-coverage transition inside the four-pixel profile window.
The new path has zero missing crossings. Native raster sampling, shared
with hybrid, has a maximum displacement of 0.337 px at both resolutions.
Its original displacement met this tolerance too; the nearest-filter
comparison detects its 110 dpi minification error independently.

## Figure size and elapsed rendering time

These are individual elapsed measurements under the machine's existing
workload. SVG output uses svglite; PNG output uses ragg.

| Backend | dpi | Original SVG bytes | New SVG bytes | Original SVG seconds | New SVG seconds |
|---|---:|---:|---:|---:|---:|
| vector | 110 | 49,763 | 244,526 | 1.43 | 2.05 |
| raster | 110 | 76,670 | 76,610 | 0.30 | 0.33 |
| hybrid | 110 | 76,670 | 76,610 | 0.40 | 0.39 |
| vector | 300 | 49,763 | 244,526 | 0.43 | 1.07 |
| raster | 300 | 202,298 | 202,298 | 0.72 | 0.55 |
| hybrid | 300 | 202,298 | 202,298 | 0.72 | 0.64 |

The vector SVG carries a 300 dpi texture image at both requested export
resolutions. Its image size accounts for the increase in SVG bytes.

The complete measurements are in `texture-edge-before/edge-metrics.csv`,
`texture-edge-after/edge-metrics.csv`, and the corresponding `metrics.csv`
files. `texture-edge-probe.R` reproduces the coloured drape exports;
`texture-edge-measure.R` reproduces the geometric edge measurements.

## Validation

The new regression checks failed nine assertions against the original
renderer at `434aaa1`, with zero test errors. The new sampler matches the
texel-clipped volcano reference exactly at both resolutions. The focused
texture suite passed before the complete suite run. I inspected the rendered
110 dpi hybrid figure and 300 dpi vector figure.

The clean full suite after rebasing onto `a751b41` produced:

```
FAILED 0 ERRORS 0 PASSED 10399 SKIPPED 1 WARNINGS 0 TESTS 1017
BASELINE REGRESSIONS FAILED 9 ERRORS 0
BYTE_IDENTICAL_UNTEXTURED 45
BYTE_IDENTICAL_VECTOR_PNGS 45
NEUTRAL_RASTER_LIGHTING_IDENTICAL 2
BYTE_IDENTICAL_TEXTURES_ACROSS_BAND_CUTS 4
```

The optional Asymptote renderer was unavailable. The native-SVG hover test
and texture SVG browser checks ran successfully. I ran the clean suite from
the junction path with `R_MAX_VSIZE=4Gb`; free RAM at launch was 20.0 GiB.

The untextured comparison covers serialized compiled vertices, fragments,
drawing order and native raster pixels for all 15 referee fixtures across
three cameras. The vector check compares the original and current ragg PNG
files byte for byte for those same 45 cases. Neutral ambient lighting gives
identical native checker images with lighting enabled and disabled at both
resolutions.

The binned-fill interaction check cuts 18 source faces into 30 pieces while
preserving opaque texture pixels byte for byte. It covers orthographic and
perspective cameras at 110 and 300 dpi. `texture-edge-bands.R` reproduces it.

The PDF device checks use `pdftools::pdf_convert()` in the test helper.
`pdftools` is declared in Suggests and gated by an installation check. Its
installed recursive strong dependency set contains ten packages, including
five base packages. The package's runtime dependencies are unchanged.
