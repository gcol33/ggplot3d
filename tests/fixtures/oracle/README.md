# Independent visibility oracle

`reference.R` uses the schema-1 contract and base R. It derives the model scale,
eye, right/up/depth basis, pole orientation, roll, sphere framing, automatic
distance and clipping planes from the camera specification. It uses no engine
matrix, clipping, intersection, ordering or visibility routine.

`ref_render(packet, camera, viewport, n = 160L)` returns pixel-centre NPC samples,
the nearest primitive ID (zero for background), front-to-back primitive and alpha
lists truncated at the first opaque hit, and an edge exclusion mask. Face hits
come from axial rays intersecting source polygon triangle fans with barycentric
containment. Convex polygons are clipped against box and near/far half spaces
before projection. Stroke coverage uses physical millimetres and interpolates
reciprocal axial depth under perspective. Ellipses and even-odd polygon marker
footprints have constant anchor depth. Stroke fixtures use straight paths, butt
caps and hidden policy 0. The helpers additionally support round/square caps.

The oracle covers the face, stroke and marker primitives used by the fixtures.
Joined bent strokes, hidden-policy 1/2, text/labels and overlay partitions require
additional oracle semantics before they can be scored with this implementation.

`ref_rasterise()` paints compiled fragment coverage in the returned `draw_order`
using `parent_primitive_id`, including polygon masks. It uses projected point-in-
polygon tests for faces, independently of the oracle's ray/triangle calculation.
It performs no depth sorting. `ref_score()` compares visible IDs, and compares
the complete ordered ID/alpha lists in translucent scenes. Acceptance is zero
mismatched interior pixels. Failure RDS files preserve input, camera and NPC
locations so a mismatch can be reproduced without an image baseline.

Face edge exclusions are Euclidean distances of at most one sample pixel from
projected source/clipping edges. Internal mesh triangle edges are included.
Circular marker and butt/square stroke boundaries use exact screen distances;
polygon marker rings use exact edge distances. Other ellipse and round-cap
footprint edge bands use one-pixel sample translations: 32 directions for markers
and 16 for strokes. This finite angular sampling can retain some
samples extremely close to the one-pixel limit; it never discards a farther
sample. Compiler-created BSP edges are kept in the scored region. Output files
record both total scored pixels and scored foreground pixels. Dense A fixtures
use zoom 2.5 so triangle interiors survive the boundary exclusion at 160 x 160.

Ray containment uses barycentric slack 1e-12. Parallel-ray rejection uses
64 times machine epsilon times the normal and ray-direction norms. Triangle
fans omit Gram determinants at most machine epsilon squared. Depth ties use the
contract's 64 times machine epsilon times max(1, model radius), grouped against
the nearest hit. Straight-path validation allows cross-product components at
most 1e-12. These numeric tolerances do not widen the excluded pixel band.

The oracle tests establish analytic camera axes and framing in both projections,
pole/roll orientation, axial depths of constant and tilted planes, the two halves
of crossing faces, near and box clipping, circle and butt/round stroke coverage,
even-odd footprint holes, opacity-zero removal and ordered alpha contributions.
A manually constructed compiled scene verifies that incorrect painting order
produces a nonzero score without calling the production renderer.

Run from the repository root with R 4.6.1:

```powershell
& 'C:/Program Files/R/R-4.6.1/bin/Rscript.exe' bench/run-referee.R
```

Capability-gated testthat tests skip unsupported engine claims by name. The matrix
runner also scores those scenes diagnostically, keeping the skip status and the
numeric result in separate columns.
