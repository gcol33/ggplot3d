# Scene contract, schema 1

This is the integration boundary for ggplot3d. `src/scene/contract.h` defines the
native types; `R/scene-contract.R` constructs their wire representation. These two
files are frozen during parallel phase-1 work. Contract changes go through the
orchestrator with a matching test and an explicit migration for every consumer.

## Entry and ownership

`scene_compile(packet, camera, viewport)` is internal. It validates the packet,
retains `styles` in R, and invokes `scene_compile_cpp(packet_without_styles,
camera, viewport)` once. The registered Rcpp wrapper makes one `.Call` per panel.
The native engine reads no colours, fonts, expressions, ggplot objects or style
columns and makes no R callbacks. Native interrupt checks remain permitted.
All native inputs and outputs are flat column lists. There are no per-primitive
R objects. Tables of a given kind have equal column lengths, including typed
zero-length columns for empty scenes.

The R result adds `styles` and `source` (the original packet without styles).
`source` allows backends to resolve `kind` plus `source_index` to original marker
footprints, text metrics and line policies. R copy-on-modify preserves input
values. Engine output geometry and draw order are authoritative for drawing.
The complete result is serialisable with ordinary R serialization.

`scene_layer_grob(packet)` creates the tagged, unprojected layer gTree consumed
by the panel collector; empty packets produce a ggplot2 zero grob. It performs no
rendering. `new_scene_viewport()` accepts physical millimetres, or grid units
resolved in the active viewport. Physical dimensions are resolved in the panel's
`makeContent()` method after the furniture margins have been allocated.

## Types and indexing

In the following tables, `int` means an R integer vector / C++ signed 32-bit
integer, `dbl` means an R double vector / C++ double, and `bool` means an R logical
vector / C++ boolean. NA, NaN, infinity, factors, matrices and list-columns are
invalid in geometric tables. Camera automatic values are the explicit exception.
Tables and top-level lists use the exact field order shown in the constructors.
Unknown, missing and duplicate fields error. Constructors supply documented
defaults and coerce finite exact integers and numeric coordinates to storage
types; validators do not repair packets.

All wire indices and offsets are **one-based**. Native storage converts buffer
indices and offsets to zero-based once. Identity values and enum codes retain
their values. Counts are lengths. A range `(offset, count)` occupies
`offset ... offset + count - 1`, never a terminal offset. Index capacity is
2,147,483,647; allocation/index overflow errors. Zero-length optional mask ranges
have offset 1. A source row of 0 and layer of 0 identify synthetic furniture.
These numerical limits describe the chosen storage type.

## Input packet

Top-level order is `schema_version`, `panel_id`, `style_count`, `vertices`,
`corners`, `faces`, `polylines`, `markers`, `text`, `footprints`,
`footprint_vertices`, `styles`. The native packet omits `styles` entirely.

| Field | Type | Meaning |
|---|---|---|
| schema_version | int scalar | Exactly 1 |
| panel_id | int scalar | Positive build-assigned panel identity |
| style_count | int scalar | Number of R-side styles |
| styles | R data frame | `style_id` integer keys exactly `seq_len(nrow(styles))`; remaining columns belong to R |

Style keys are panel-wide. The panel collector joins/remaps emitter-local style
keys when merging layers. Colour, fill, linetype, font, label content, ggplot2
shape, lighting flags and any backend-specific style data remain here. Each row
is a complete material for its primitive kind; unused columns may contain NA.
`opacity` is the native visibility/compositing value. R resolves colour alpha
and mapped alpha into this value before packing; backends apply it exactly once.
Line widths, footprints and text metrics are geometry and are explicitly packed.

### Shared geometry buffers

| Table | Column | Type | Units and meaning |
|---|---|---|---|
| vertices | x, y, z | dbl | Normalised expanded transformed box fractions; box boundaries 0 and 1 |
| corners | vertex | int | Reference to a row of `vertices` |
| corners | source_row | int | Original layer row for this corner; 0 for synthetic corners |
| footprint_vertices | x_mm, y_mm | dbl | Physical offsets from the anchor before footprint rotation |
| footprint_vertices | ring | int | Positive ring ID scoped to the owning footprint, contiguous within each ring |

Input vertices have already passed the position-scale lifecycle. For a transformed
axis range `[lo, hi]`, the coordinate fraction is `(value - lo)/(hi - lo)`.
Scale transforms, discrete mapping, boundary sentinels, missing-value policy and
collapsed-range expansion are R responsibilities. These are never repeated in
the engine. Values outside [0, 1] are legal and support geometric clipping and
`clip_data = FALSE`. Aspect is applied by the engine exactly once.

The same vertex can appear in many corners. Each corner belongs to its referring
primitive for provenance purposes. The collector adjusts corner/vertex offsets
when concatenating buffers. Tables may share vertex and corner storage. Identity
and style joins use explicit keys; source data row order is never a join key.

### Primitive tables

`faces`, `polylines`, `markers`, and `text` start with these columns in this order:

| Column | Type | Meaning and constructor default |
|---|---|---|
| primitive_id | int | Positive unique identity across every kind in this panel; generated consecutively in kind order if omitted |
| layer_id | int | Nonnegative build layer identity; default 1 |
| source_row | int | Stable original source row, or representative row of generated geometry; default table row number |
| style_id | int | R-side material key, 1 through style_count; default 1 |
| offset | int | First row in `corners`; required |
| count | int | Number of corners; defaults face 3, polyline 2, marker/text 1 |
| role | int | Coplanar order: fill 0, boundary/contour stroke 1, marker 2, text 3; defaults by kind |
| partition | int | Underlay 0, depth scene 1, overlay 2; default 1 |
| opacity | dbl | Effective alpha in [0, 1]; default 1 |

Primitive IDs identify the emitted geometry. Multiple triangles may have the same
`source_row`; they must have distinct `primitive_id`. R may retain a keyed mapping
from those IDs to mesh/feature/part/ring/face identifiers outside the engine.
Explicit IDs are checked globally even when constructor-generated IDs are mixed in.

Faces are triangles or simple **convex planar** polygon rings with at least three
corners, without a repeated closing corner. Concave polygons and holes are
triangulated by the emitter with preserved original boundary edges. Polygon
winding is counterclockwise viewed from its outward normal in right-handed model
xyz. The engine preserves winding, validates coplanarity and convexity, and
computes a unit normal after aspect scaling. Both sides remain drawable.
Lighting can orient a copy of the normal toward the eye; topology keeps its sign.

Polylines have at least two corners, ordered along the source path. Separate
primitives represent NA breaks and material changes. Additional columns:

| Column | Type | Meaning and default |
|---|---|---|
| width_mm | dbl | Full physical stroke width, nonnegative; 0 |
| mitre_limit | dbl | Dimensionless ratio >= 1; 10 |
| cap | int | Butt 0, round 1, square 2; 0 |
| join | int | Round 0, mitre 1, bevel 2; 0 |
| hidden | int | Hide 0, dashed 1, show 2; 0 |
| arrow_start, arrow_end | bool | Requested original endpoint heads; FALSE |

Arrow dimensions and dash patterns are R-side style fields. Their physical
coverage must also be represented by additional marker/face primitives when it
affects visibility. Endpoint flags prevent creation of arrows at a split.

Markers have exactly one corner and add `footprint_id` (int, required), referencing
`footprints`. A footprint describes the painted marker coverage, including
stroke extent; its anchor depth is constant across this camera-facing shape.
Composite markers can use multiple primitives with shared source/style identity
and distinct primitive IDs. They must have disjoint interiors when translucent.

Text and labels have exactly one corner and add the following columns. Label
content and padding/rectangle styles remain in `styles`; their measured full
extent is represented here. Visibility of text/labels is decided at the anchor
as a whole, with one compositing depth for the full label.

| Column | Type | Meaning and default |
|---|---|---|
| width_mm, height_mm | dbl | Full measured physical dimensions, nonnegative; 0 |
| hjust, vjust | dbl | Anchor fractions of the unrotated rectangle; 0.5 |
| angle | dbl | Counterclockwise screen rotation in degrees; 0 |

### Footprints

| Column | Type | Meaning |
|---|---|---|
| kind | int | Ellipse 1 or polygon 2 |
| offset, count | int | Range in footprint_vertices; ellipse uses offset 1, count 0 |
| radius_x_mm, radius_y_mm | dbl | Nonnegative ellipse radii; unused for polygon |
| angle | dbl | Counterclockwise screen rotation in degrees |

Polygon rings use the even-odd fill rule and physical offsets relative to the
anchor. Each ring has at least three vertices; closing vertices are implicit.
Ring orientation does not determine holes. Ellipse rotation applies to its local
radii. This supports exact circular marker coverage without choosing a tessellation
resolution in the contract. Backends retain the glyph and intersect it with
returned masks. Device support for these operations is an explicit capability.

## Camera and viewport

`new_camera_3d()` returns an immutable-by-convention list with these fields:

| Field | Type | Units and constraints |
|---|---|---|
| projection | string | orthographic (default) or perspective |
| azimuth | dbl scalar | Degrees about +z; 0 puts eye on +x, positive toward +y; default -60 |
| elevation | dbl scalar | Degrees above xy, [-90, 90]; default 30 |
| roll | dbl scalar | Counterclockwise rotation of the projected content; default 0 |
| distance | dbl scalar | Longest-box-side units; Inf for orthographic, positive finite or NA for automatic perspective |
| fov | dbl scalar | Perspective vertical field of view in degrees, (0, 180); default 35 |
| zoom | dbl scalar | Positive screen magnification; default 1 |
| target | dbl length 3 | Normalised box fractions; default c(0.5, 0.5, 0.5) |
| aspect | dbl length 3 | Positive relative model side lengths; default c(1, 1, 1) |
| near, far | dbl scalar | Positive finite camera-depth planes, or NA for automatic; near < far after resolution |
| clip_data | bool scalar | Clip geometry against the six normalised box planes; TRUE |
| clip | bool scalar | Clip against the physical scene rectangle; TRUE |

Only constructors accept NULL for automatic distance/near/far; the wire uses NA.
`aspect = "data"` and public `clip = "on"/"off"` are resolved in Coord3D before
this boundary. Camera values must be numerically representable after projection.

Model coordinates equal box fractions times `aspect / max(aspect)`. Model xyz
is right-handed with z up. The eye looks toward target. Screen x points right,
screen y points up. Camera z is positive depth away from the eye; this makes the
right/up/depth basis left-handed. Depth is axial eye distance, never Euclidean
distance to the eye. At the poles, azimuth still fixes the right/up basis.

A sphere centred on the requested target encloses all eight model box corners.
Orthographic framing fits this sphere into both physical viewport dimensions,
giving rotation-invariant magnification. Its finite reference eye distance is
`2 * radius + 1` in model units. Inf is never inserted in a matrix. Automatic
perspective distance fits the sphere within the smaller horizontal/vertical half
angle: `radius / sin(half_angle) + tolerance`. Explicit perspective distance is
honoured even when it puts the eye inside the scene. Near-plane clipping precedes
division. Zoom scales projection about the viewport centre.

Automatic near is `max(tolerance, distance - radius - tolerance)`; automatic far
is `distance + radius + tolerance`. Explicit planes override these individually,
then their resolved order is checked. Near/far clipping applies even with both
box and rectangular clipping disabled, and applies to overlays too.

Viewport fields are double scalars `width_mm`, `height_mm`, `dpi`, all positive
and finite. They describe the inner scene viewport. Millimetres convert to device
pixels with `mm * dpi / 25.4`; a raster adapter records its integer rounding.
NPC coordinates use (0,0) at the lower left and (1,1) at the upper right of this
viewport. NPC can exceed [0,1] when rectangle clipping is disabled. Physical
screen coordinates are `(x_npc * width_mm, y_npc * height_mm)`.

## Compiled result

Native top-level fields are `schema_version`, `panel_id`, `vertices`, `fragments`,
`weights`, `masks`, `draw_order`, `camera`, `viewport`, `capabilities`. The R wrapper
appends `styles` and `source`. All output tables are column lists, not data frames.

| vertices column | Type | Meaning |
|---|---|---|
| x, y, z | dbl | Normalised box coordinates, including new intersection vertices |
| x_camera, y_camera | dbl | Signed camera right/up displacement in model units |
| depth | dbl | Positive camera depth in model units |
| x_npc, y_npc | dbl | Projected coordinates in the inner physical viewport |
| clip_w | dbl | Perspective depth, or 1 for orthographic |

Each fragment owns a contiguous range of output vertices. Faces are polygon
rings; lines are ordered centre lines; marker/text geometry retains its original
anchor even when a clipping mask trims its screen coverage.

`fragments` has the nine common primitive columns above, followed by:

| Column | Type | Meaning |
|---|---|---|
| fragment_id | int | Unique positive output identity; row number |
| kind | int | Face 1, polyline 2, marker 3, text/label 4 |
| parent_primitive_id | int | Original input primitive_id, never an intermediate split ID |
| source_index | int | Original row in the input table selected by kind |
| mask_offset, mask_count | int | Range in masks; count 0 means no additional mask |
| depth_min, depth_max | dbl | Tight axial depth interval of output geometry |
| depth_centroid | dbl | Arithmetic mean of output vertex depths; a sorting heuristic |
| normal_x, normal_y, normal_z | dbl | Aspect-scaled unit face normal; zero for other kinds |
| path_start_mm, path_end_mm | dbl | Cumulative projected source-path arclength at fragment endpoints; zero for other kinds |
| clipped | bool | Geometry or coverage changed by clipping or splitting |
| hidden | bool | Hidden stroke portion retained by dashed/show policy |
| arrow_start, arrow_end | bool | Original requested endpoint still belongs to this fragment |

Common `offset/count` now reference output `vertices`. `primitive_id` equals
`parent_primitive_id`, and may repeat across fragments. Role, partition, layer,
source, style and opacity retain their source values. Splitting a split fragment
continues to refer directly to its original input primitive.

Dash arclength is physical projected length along the source path after near/far
clipping and before box, rectangle or occlusion clipping. Invisible portions
within the near/far slab still advance the phase. Portions outside that slab
contribute zero. Perspective interpolation uses projected physical lengths.
Fragment endpoints retain these cumulative values across subsequent BSP cuts.

`weights` columns are `vertex` (int output vertex index), `corner` (int original
input corner index), `weight` (dbl affine coefficient). For every output vertex,
weights sum to 1 and reconstruct its normalised xyz from the input corner
coordinates. Untouched vertices have one coefficient of 1. Repeated cuts combine
coefficients back to original corners. Triangle coefficients are barycentric;
polygon coefficients are sparse affine weights from the sequence of cuts.
Interpolation is performed in model space before perspective division. This
preserves arbitrary future per-corner attributes without transferring colours.

`masks` columns are `x_npc`, `y_npc` (dbl), `ring` (int). Each mask is an even-odd
path; rings are local to its range. Masks intersect a fragment's original painted
coverage. Multiple fragments of a translucent glyph use disjoint mask interiors
so that alpha is applied once per covered point. Masks also permit thick strokes
to retain their centre line while clipping their painted outline.

`draw_order` is an int permutation of fragment row indices, back to front. It
spans every layer in the panel. Underlays precede the depth scene, overlays follow
it. Underlay/overlay layer order is explicit; depth-scene layer IDs participate
only in coplanar ties. Coplanar order is `(role, layer_id, source_row, fragment_id)`.
Backend batching combines consecutive compatible runs while preserving this
permutation. Reordering all faces or all markers into a single kind-wide batch
would violate it.

`camera` contains 4x4 double matrices `view`, `projection`; double vectors `eye`,
`right`, `up`, `forward`, `model_scale`; and double scalars `distance`, `near`,
`far`, `view_height`, `tolerance`. Matrices multiply column vectors and are stored
column-major as R matrices. Apply model_scale to normalised xyz, then view, then
projection. Divide homogeneous xyz by w; xy is in [-1,1] before the NPC mapping
`(xy + 1)/2`, and z is in [0,1] between the clipping planes. `view_height` is the
orthographic base height after zoom; perspective uses fov and distance.
`viewport` echoes the validated physical viewport.

## Guarantees and current capability

Schema-1 geometry tolerance is `64 * DBL_EPSILON * max(1, radius)` in model units,
recorded in the resolved camera. It governs degeneracy and planar classification;
clipping uses signed half-space distances and interpolated exact boundary
crossings. It does not perturb camera or geometry to force an ordering. The engine
agent must measure almost-coplanar cases and report any proposed change to this
numerical rule before changing the frozen contract.

The scaffold validates storage, ranges, enum values and references independently
on both sides of the native boundary. Native face validation additionally checks
planarity, convexity and nonzero area. The compiler never mutates the input. It
supports orthographic/perspective projection, box/near/far/rectangle clipping of
faces and line centre lines, anchor clipping, provenance reconstruction, and
deterministic back-to-front centroid ordering across layers. It returns typed
empty output for fully removed scenes. Zero-opacity primitives generate no draws.

`capabilities` is `list(visibility = "centroid", exact_visibility = FALSE,
footprint_clipping = FALSE)` in this scaffold. Lines are emitted as segments.
Masks are empty. Marker and text clipping currently tests the anchor; physical
marker/stroke coverage is parsed and retained for the engine implementation.
There is no claim of correct intersecting-face, cyclic, hidden-line, silhouette,
or translucent-overlap visibility here. The centroid value is a heuristic and
does not establish a valid ordering for such geometry.

The phase-1 engine must return `visibility = "bsp"`, exact visibility and footprint
clipping capabilities only after referee acceptance. It must split filled faces
to resolve crossings and cycles, apply physical footprint rules, preserve path
phase and provenance, and composite translucent fragments back to front. Opaque
faces block hidden-line/anchor visibility; transparent faces composite with other
primitives in the same resolved order. Text follows the whole-anchor rule.
Backends consume this same geometry, camera and visibility contract. Capability
errors identify the unsupported operation; backend choice remains explicit.

Geometry caching keys must include schema, coordinates, topology, aspect,
clipping and geometry-affecting primitive properties. View-dependent cache keys
also include resolved camera, physical dimensions, DPI, opacity/visibility policy
and measured footprints. Colours can change independently. The scaffold installs
no cache and promises no performance measurements.

## Resolutions of the design notes

- The task's settled `_3d` layer/stat names override the unsuffixed examples in 07.
- 07's material buffer is retained on the R side. Only style keys, effective alpha
  and physical coverage cross the native boundary. This resolves Q13's flat-table
  boundary alongside its requirement that the engine avoid R style handling.
- The fifth logical primitive, label, shares the text anchor contract with its
  measured box and R-side style; it needs no fifth geometric engine.
- 07's barycentric provenance generalises to sparse original-corner weights for
  polygon fragments. This makes split results unambiguous for non-triangular faces.
- 07's exact visibility is the phase-1 acceptance target. The explicitly requested
  centroid implementation reports its limited capability in the returned value.
- Positive roll is defined by counterclockwise projected content, resolving the
  possible ambiguity between rotating a camera basis and rotating the image.
- The phase-1 scale bridge follows 07 section 6. The older notes' call-frame,
  per-layer drawing and variable panel-aspect sketches do not define this boundary.
  Panes that can occlude data enter the shared depth scene.
