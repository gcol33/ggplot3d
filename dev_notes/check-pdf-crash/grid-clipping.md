# Grid clipping reduction

Baseline: 13c4f58, Windows R 4.6.1. The generated examples file contains
136 topics with examples. profile-examples.R counts grid resolveClipPath()
calls with an empty device reference, rectangular clipping viewports and
grob masks. Topics run unchanged on separate 7 inch ragg devices at 100 dpi,
with the same examples file, dimensions and random seed.

| Topic | Before | After |
| --- | ---: | ---: |
| stereo_3d | 52,860 | 0 |
| geom_quiver_3d | 4,174 | 0 |
| geom_hex_3d | 3,688 | 0 |
| stat_biplot_3d | 3,311 | 28 |
| coord_globe_3d | 2,885 | 0 |
| facet_slices_3d | 2,709 | 6 |
| stat_parametric_geodesic_3d | 1,758 | 0 |
| geom_edge_3d | 938 | 0 |
| outline_3d | 760 | 0 |
| geom_contour_3d | 503 | 0 |
| backend_3d | 55 | 54 |
| Total | 78,013 | 98 |

The remaining definitions are 44 native text clips and 54 hybrid visibility
region clips. The largest remaining topic is backend_3d, with 54.

## Sources of clipping

render_scene_grid() groups adjacent pieces of one glyph, builds their
fragment-mask compound path with mask_path_3d(), and gives each draw run a
clipped viewport. Outlined meshes create many separately ordered stroke
runs; whole polylines can carry many footprint pieces.

Faces already have clipped vertices. The seam underlay and its per-colour
compound paths use filled geometry. Translucent seam groups and sheets use
uniform alpha masks on devices supporting the required compositing operation.
The rectangular inner panel clip uses one viewport per panel. Hybrid
visibility regions enter through clip_grob_3d().

## Painted geometry

src/grid-paths.cpp constructs stroke outlines from segment strips, endpoint
caps and join sectors. Mitres bevel at the requested ratio. Circular
boundaries share one sampler, bounded to a sagitta of 1/4096 device pixel.
Polygon operations use the package's existing bundled Clipper2 library,
with coordinates quantised to 1e-8 millimetres.

A primitive's painted outline is cached for the current render. Native
external pointers hold its paint groups; every visible run intersects its
mask with those groups. Separate dash paths preserve their separate paint
operations. The resulting rings are filled as compound grid paths. If
adjacent offset joins cross, the device's native stroke is retained under
an alpha or luminance mask. A whole-mask pixel comparison verifies this
case against native drawing.

Marker shapes 1 and 15--21 share paint_metrics_3d() and
marker_footprints_3d() with scene visibility. Circular markers preserve
their separate fill and border paints, including translucent overlap.
Fill and border semantics follow the [R graphics engine symbol definitions](https://github.com/wch/r-source/blob/trunk/src/main/engine.c).
Other marker shapes and native text retain their device clipping when
required. Surface underlays and scene draw order keep their existing
handling. The scene schema and raster backends are unchanged.

Converting only solid straight segments reduced the total to 3,272, which
still reproduced the PDF crash at dev.off(). Complete painted outlines
remove the remaining high-volume stroke and marker clips.

## Visual verification

render-comparison.R replays 55 compiled scenes from the grid-backend,
grid-opacity and grid-parity tests on ragg PNG and Cairo PDF devices.
It also renders all 21 gallery chunks on both devices. Cairo PDF permits
capturing the baseline through the standard PDF device's buffer failure.
compare-images.py rasterises PDFs with Poppler and compares 288 image pairs,
including 136 example PNGs. Side-by-side sheets were inspected.

Geometry, colours, occlusion and seam-free surface fills are preserved.
Stroke-edge antialiasing differs: a filled boundary has one coverage pass;
a native stroke clipped at the same boundary multiplies two coverage values.
The dense stereo wireframe shows stronger edge coverage. The exact pixel
measurements are in image-comparison.csv.

Tests cover footprint holes, nonconsecutive ring IDs, cap and join geometry,
mitre limits, cached outline equivalence, separate paint groups and clipped
marker transparency. A translucent marker whose mask leaves it whole agrees
with native grid output within three 8-bit colour units across the tested
circle and polygon symbols; shapes 19 and 20 retain a two-unit bound.

## Validation and reproduction

Use R_MAX_VSIZE=4Gb and run R commands sequentially. After native interface
changes, regenerate Rcpp attributes and documentation. Then run:

    Rscript --vanilla dev_notes/check-pdf-crash/profile-examples.R after path/to/ggplot3d-Ex.R
    Rscript --vanilla dev_notes/check-pdf-crash/test-grid.R
    Rscript --vanilla dev_notes/check-pdf-crash/test-all.R
    Rscript --vanilla dev_notes/check-pdf-crash/run-check.R

run-check.R also accepts an existing source tarball as its first argument.
It uses x.Rcheck inside the clone. The deeper diagnostic directory produced
262- and 264-character vignette SVG paths; x.Rcheck reduces those paths to
239 and 241 characters, and vignette rebuilding succeeds there.

example-clips.csv records the complete before/after counts. The local before,
after and native-all directories hold rendered images and raw measurements.
The baseline backend copy and captured scene RDS files are local diagnostic
artifacts.

After rebasing onto 77989c3, a clean native rebuild and full test suite passed
9,083 assertions with zero failures, errors or warnings. Two environmental
skips cover unavailable Asymptote and a headless Chrome debugging port that
did not open. No protected raster, hybrid or scene-contract source differs
from that upstream revision.

The standard PDF examples in the final CRAN check completed in 221 seconds,
including device closure, and produced a 7,579,646-byte PDF. Their timing
is reported as a check note.

R CMD check --as-cran --no-manual completed with zero errors, zero warnings
and two notes on the 77989c3 base: incoming metadata/external links and
example runtime. Installation, examples, tests and vignette rebuilds passed.

After integrating the homogeneous-space, observer-camera and smooth-shading
changes through b2e34f2, bindings and documentation regenerated without
additional changes. A clean native rebuild and full suite passed 9,230
assertions with zero failures, errors or warnings. The sole skip is the
unavailable Asymptote renderer.
