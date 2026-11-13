# Phase 1 work plan

Build the exact panel scene, full z position lifecycle, and vector backend against
schema 1 in `08-scene-contract.md`. The orchestrator starts four agents in parallel
on main. Each agent reads 07, 08, this plan and the competitor report. No agent
commits. Each reports changed files, test commands/results, remaining acceptance
failures and requested shared-file edits. Every public geom/stat constructor uses
the `_3d` suffix.

## File ownership

| Owner | Exclusive editable files |
|---|---|
| A: Engine | `src/**` except frozen `src/scene/contract.h`; `tests/testthat/test-engine-*.R` |
| B: Integration | `R/coord-3d.R`, `R/layout-3d.R`, `R/scale-z.R`, `R/build-adapter.R`, `R/emitters.R`, `R/layers-*.R`; `tests/testthat/test-integration-*.R` |
| C: Grid and furniture | `R/backend-grid.R`, `R/panel-furniture.R`, `R/guides-3d.R`, `R/theme-3d.R`; `tests/testthat/test-grid-*.R` |
| D: Referee | `tests/testthat/test-referee-*.R`, `tests/fixtures/**`, `inst/bench/**` |
| Orchestrator | All remaining files, including DESCRIPTION, NAMESPACE, generated `R/RcppExports.R`, `man/**`, package documentation, verification scripts and shared test helpers |

`R/scene-contract.R`, `src/scene/contract.h`, and `08-scene-contract.md` remain
frozen. Agents propose schema amendments to the orchestrator; they never edit
these files independently. A owns `src/RcppExports.cpp` but must keep the single
native entry signature. Only the orchestrator runs compileAttributes/document
when all agents' writes are complete, because these commands touch shared files.
Agents use existing wrappers while testing native changes. Dependencies, imports,
method registration, `.onLoad()` hooks and roxygen-generated exports are listed
in reports for orchestrator integration. New helpers stay in the owner's files.

No two agents own a test file. D's fixture builders and oracle live under
`tests/fixtures/`; D supplies an explicit loader in each referee test. A/B/C keep
their small local fixtures within their own test files, avoiding shared helper
edits. They can read D's committed fixture data when available.

## Common R hand-offs

These internal interface names and argument contracts coordinate B and C:

| Provider | Interface | Consumer and result |
|---|---|---|
| B | `emit_scene_3d(data, params, panel_params, layer_id)` | Registry dispatcher returning a schema-1 packet with local IDs/styles; used by adapted geom draw methods |
| B | `merge_scene_packets(packets, panel_id)` | Keyed remapping of vertex/corner/footprint/style/primitive IDs; accepts typed empty packets; C's collector uses it |
| B | `collect_scene_layers(panel, panel_params, theme)` | Classifies tags and explicit facet decorations; returns `list(packets, underlay, overlay)`; unsupported nonempty grobs error |
| C | `scene_panel_grob(packets, panel_params, theme, underlay = list(), overlay = list())` | B's Coord3D panel hook; returns a deferred grid gTree |
| C | `panel_furniture_3d(panel_params, theme, viewport)` | Returns `list(packet, overlay, viewport)`; packet has panes/grid/back edges, overlay has axis/front-frame decoration, viewport is the fitted inner viewport |
| C | `render_scene_grid(scene, viewport)` | Returns a grid gTree for a compiled scene, preserving draw_order |
| C | `setup_guides_3d(panel_params, guides, theme)` | Returns completed panel_params with trained 3D position guides |

Completed `panel_params` is a build-local list containing named x/y/z view scales,
`panel_id`, `scale_ids` (named integer x/y/z), `ranges` (named expanded transformed
range pairs), `camera` (`new_camera_3d()` value), `axis_titles` (named x/y/z),
`axis_guides` (named x/y/z and optional secondary guides), and `coord_params`
(immutable axes/panes/box/light/backend/margin settings). B builds this value and
C consumes it. No guides or scene buffers are stored on the user-owned coord.

Collection occurs at Coord3D's panel hook after layer grob creation. C resolves
physical dimensions and furniture, B merges geometry with explicit key remapping,
and C calls `scene_compile()` once for the whole panel before rendering. Geometry
requiring measured footprints is finalised at this physical-size boundary. No
independent layer compilation is allowed. Projected corner/axis queries use the
same native projection through the scene interface; adapters must never install
a second camera formula. The engine API can batch furniture/guide anchors into
the panel packet so their projected positions are returned in that one call.

For deterministic framing C can measure label dimensions before compilation,
reserve margins, and pack cube-edge/guide-anchor candidates together. Edge
selection and overlay placement consume those projected anchors. If the full
fitting strategy needs additional native projection queries, report that conflict
to the orchestrator with a concrete batched protocol before implementing it.

## A: Engine

Goal: replace scalar ordering with exact geometric visibility at the shared native
boundary and measure its scaling to 100,000 and 1,000,000 input faces.

Consumes schema-1 packet/camera/physical viewport. Provides the existing
`scene_compile_cpp(packet, camera, viewport)` output with unchanged column types,
units and provenance. Factor the engine into native projection, clipping,
partitioning and coverage modules under `src/scene/`; keep one implementation of
each calculation. Pure native tests can be compiled into src and invoked through
ordinary scene fixtures; do not add a second public/native renderer API.

Work:

1. Complete shared projection and clipping for near/far, six box planes and the
   viewport; audit pole views, explicit inside-box eyes, aspect and roll.
2. Build a model-space BSP with plane classification and polygon splitting. Use
   spatial acceleration and deterministic insertion heuristics. Resolve cycles by
   splitting geometry; retain all precedence constraints. Report fragment growth
   and peak memory alongside time. Geometry intersection and BSP traversal must
   preserve original-corner weights and source keys.
3. Integrate marker footprints, thick stroke outlines and whole-anchor text
   visibility. Resolve face intersections and silhouette coverage. Preserve
   cumulative dash phase and endpoint arrows. Transparent faces participate in
   ordered compositing and do not act as opaque hidden-line blockers. Apply the
   same rules to panes and grid geometry.
4. Implement coplanar tie policy and numerically measured tolerances. Expose exact
   capabilities only when the corresponding fixtures pass.
5. Add reusable model-partition and projected-coverage cache keys. Include every
   geometry/visibility input identified in 08. Colour edits may reuse geometry;
   transformed data, topology, aspect, clipping and scale/view changes invalidate
   their corresponding entries. Cache lifetime must be explicit and bounded by
   owning scene/build objects; avoid process-global mutable scene state. Propose
   any returned cache metadata as a schema amendment first.

Acceptance:

- Analytic two-face crossings, cyclic strips, coplanar surface/contour ties,
  almost-coplanar faces and near-plane crossings match D's independent oracle.
- Reversed layer order preserves visible geometry except declared coplanar ties
  and overlays. No deleted cycle edges, centroid-only decisions or unexplained
  perturbations remain in the exact path.
- All generated vertices reconstruct from original corners; normals, intervals,
  masks, arrow flags and dash phases remain consistent through repeated splits.
- Orthographic and perspective matrices reproduce emitted NPC coordinates at
  square and unequal physical viewport dimensions.
- Large-scene measurements report compile time, peak memory, candidate counts,
  output fragments and cache reuse. No unmeasured performance claim is accepted.
- Native unit tests are exercised from `test-engine-*.R`, including direct native
  malformed-input cases. Existing phase-0 contract tests continue to pass; update
  their capability expectations only through the orchestrator.

## B: Integration

Goal: make z participate in ggplot2's complete position lifecycle and collect
registered layers into one scene without mutating ggplot2 or the source plot.

Consumes the frozen R packet constructors, camera value and C's panel/guides
interfaces. Provides Coord3D, Layout3D, build-local Scales3D, emitter registry and
the common R hand-offs above. Use the installed ggplot2 build source as the actual
interface reference; 07 section 6 defines the intended lifecycle.

Work:

1. Implement plot-class dispatch and fresh build context; bind the actual cloned
   scale collection through its clone method, delegate to the registered parent
   ggplot_build method, and isolate compatibility coupling in build-adapter.R.
2. Implement z/zmin/zmax/zend/zintercept transform, train, map, reset, retrain and
   guide lifecycle, including fixed coordinates before stats, inferred scales,
   infinity sentinels, discrete remapping and date/time variants. Exclude the whole
   z family from non-position mapping. Join PANEL to SCALE_Z explicitly.
3. Respect scale limits before statistics, coordinate zoom after statistics,
   after_stat retransform semantics, facet shrink and free z scales. Include the
   x/y/z scale triple in panel/guide identity.
4. Implement the emitter registry and first public layers: geom_point_3d,
   geom_path_3d, geom_segment_3d, geom_text_3d, geom_surface_3d, geom_polygon_3d,
   and the corresponding surface stat where needed. Ordinary supported ggplot2
   geoms use the same emitters via the build adapter. Preserve grouping and source
   keys; resolve missing coordinates consistently. Polygon rings/holes must be
   triangulated once with a native/structured geometry implementation and a
   documented algorithm audit. Surface grid connectivity is deterministic.
5. Emit immutable layer packets and implement keyed merging. Reject unsupported
   nonempty grobs and positions explicitly. Coordinate replacement delegates to
   ordinary 2D building.

Acceptance:

- H log segment has transformed endpoints 1 and 2 for input 10 and 100; reversed
  discrete z limits place levels at their labelled positions.
- Stat probes see scales$z, transformed input and pre-stat scale OOB results;
  coordinate zoom preserves computed statistics. Bounds/endpoints transform once.
- Fixed/free facets, missing facet variables, shrink modes and margins produce
  correctly keyed independent ranges. No row-order joins occur.
- Namespace-only construction, coord/scale addition order, nested/repeated builds,
  late-added layers and coord replacement preserve source object values.
- A/B/C/G fixtures produce one panel packet with stable topology/style/source keys.
- Geom-specific checks cover path breaks, segment defaults, missing grid cells,
  surface duplicate coordinates, planar polygon holes and fixed z parameters.

## C: Grid backend and furniture

Goal: draw the compiled scene in vector form with physical-size-aware axes,
stable camera framing, and ordinary ggplot2 theme/legend behaviour.

Consumes B's completed panel_params, packet merger and collector, plus the frozen
compiled scene. Provides the C interfaces above and grid makeContent method.
Register requested methods/theme hooks through the orchestrator's shared edits.

Work:

1. Implement deferred panel compilation and backend dispatch. Emit one vectorised
   grob per consecutive compatible primitive run, using polygon/path IDs and
   vectorised marker/text calls. Preserve draw_order across kinds and styles.
2. Apply clipping masks, line dash offsets and arrow endpoint rules from the
   compiled scene. Resolve styles by style_id, never output row position. Keep
   lighting in the style finishing stage using engine normals.
3. Build cube panes, grids and rear edges as depth-scene geometry; axis text and
   requested front edges are decoration overlays. Respect underlay/overlay order.
4. Implement deterministic exterior edge selection, collapsed-axis suppression,
   physical label/title measurement, reserved margins, secondary axes, and errors
   when the fitted scene viewport is nonpositive. Preserve model aspect and
   rotation-invariant magnification inside the outer panel aspect of 1.
5. Register the 07 theme inheritance tree, axis guides and title precedence.
   Ordinary legends keep unlit mapped values. Backend capabilities are explicit.

Acceptance:

- Render and self-inspect PNG screenshots of PDF/SVG exports as well as screen
  output; test several viewport sizes, roll, pole views, long labels and facets.
- No reordering across primitive runs, split style loss, spurious split arrows,
  cumulative dash resets, doubled translucent mask coverage or scale repetition.
- Eight independent azimuth views retain intended physical magnification. Unequal
  panel dimensions preserve aspect; axis titles/ticks clear facet strips and panel
  bounds. theme_void and element_blank remove the intended furniture.
- Scene panes can occlude data in underside views. Overlay labels remain overlay
  annotations; depth-tested labels follow the anchor rule.
- Batch/grob counts scale with compatible runs, with recorded vector output size
  and device limitations. Do not hide missing clip-path capability.

## D: Referee and benchmarks

Goal: independently score correctness and performance, including adversarial
scenes where a scalar depth order cannot work.

Consumes public build results when available and schema-1 packets/compiled scenes.
Provides reproducible fixture inputs, an independent reference renderer, metrics
and benchmark scripts. The oracle lives in `tests/fixtures/ray-reference.R` or
separate code under that directory. It must not call the production engine for
ray/triangle intersections, clipping, ordering, visibility or compositing. A
test-only native oracle is permitted, with its own source and explicit loading.

The oracle casts a ray for each sample pixel, intersects source triangles with an
independently derived algorithm, orders all hits by axial depth and composites
alpha independently. It handles line/marker coverage and whole-anchor text rules
with measured geometry. Analytical camera fixtures verify any shared camera
matrix inputs, preventing an engine projection error from validating itself.
Compare interiors separately from device-antialiasing boundary bands; record the
sampling rule and any tolerance rather than silently discarding difficult pixels.

Required fixtures:

- A: intersecting surfaces in separate/reversed layers and in one grouped layer.
- B: opaque plane, points above/below, silhouette-straddling markers and crossing
  narrow/wide paths, including hidden-line policies and endpoint arrows.
- C: tilted plane intersecting columns, with explicit box-face input fixtures
  before the public column layer exists.
- G: crossing quads and cyclic separated strips, with analytic front-face regions.
- H: log endpoints, reversed discrete z, pre-stat limits, scales$z and free facets.
- Near/far-plane crossings, inside-scene perspective eyes, box crossings,
  coplanar contours, near-degenerate planes, translucent intersecting faces,
  translucent markers/strokes, whole-anchor text, and opaque/alpha-zero coverage.

Benchmark scripts compare the installed ggcube **0.3.0** with ggplot3d at 10,000,
100,000 and 1,000,000 faces. Verify installed versions and record session/toolchain,
hardware, data seed, face/corner counts, sorting mode and rendering settings.
Report build, scene compilation, gtable assembly, device writing and direct ggsave
separately. Measure median timings over declared repeats, peak memory, fragment
growth, grob count and SVG/PDF bytes. Run timings without concurrent rendering
jobs. Honour hardware-nodes rules for long or detached benchmark launches.

Keep equivalent easy painter-order scenes separate from exact-intersection
fixtures. The competitor's large-scene painter timings measure different work
from exact BSP splitting. Report unsupported cases and resource failures with
their actual diagnostics. Do not substitute invented estimates for an uncompleted
1,000,000-face run, or turn a performance comparison into a correctness claim.

Acceptance:

- Oracle agrees with analytic small fixtures before it scores either renderer.
- A/B/C/G show correct front geometry independent of layer order; H tests actual
  stat input and mapped endpoints as well as rendered positions.
- Engine-versus-oracle disagreement includes a reproducible input and pixel/ray
  location. Residual failures are surfaced; no skipped assertion or regenerated
  expected image is used to conceal a visibility defect.
- Correctness results include rendered/self-inspected evidence and numerical
  region/coverage scores. Benchmarks retain raw data and reproducible commands.

## Orchestrator integration and completion

1. Review the four reports and reconcile requested contract changes before merging
   interfaces. Apply dependency/registration/roxygen edits centrally. Confirm all
   declared dependencies have actual code uses and every exported layer has `_3d`.
2. Run compileAttributes, document, load_all, all testthat tests and R CMD check.
   Check the single native registration and the frozen wire schema against all
   consumers. Inspect representative vector device screenshots personally.
3. Review correctness against D's independent oracle, then performance and cache
   invalidation evidence. Record any unmet target explicitly; exact visibility
   remains disabled until its acceptance cases pass.
4. Review and commit only after the combined result is concrete and validated.

The raster backend is a further implementation lane consuming the same compiled
scene/matrices and capability contract. These four agents establish its shared
boundary and transparency/text tests. They do not claim raster/vector equivalence
before an actual raster adapter passes those tests.
