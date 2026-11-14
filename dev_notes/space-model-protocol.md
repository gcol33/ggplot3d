# Space/model protocol (#13)

`coord_3d(space, model, input, isometry, munch_tolerance,
munch_max_depth)` binds one space to each panel. `Space3D` is an internal
ggproto extension protocol. The default `SpaceEuclidean` leaves the
existing emission path unchanged, including normalized Euclidean distances
in `Coord3D$distance()`. The space's own distance is in intrinsic units.

## Extension contract

- `charts`, `models`, `default_model`: supported input and display names.
- `to_canonical(data, chart)`: scale-transformed input data to a finite
  numeric row matrix. All input columns are available, including additional
  ambient coordinates supplied by a specialized stat or emitter.
- `from_canonical(X, model)`: canonical points to three model coordinates,
  or four homogeneous coordinates (x, y, z, w) with w >= 0.
- `model_ranges(model, panel_params)`: named x/y/z intervals in model units.
  This can fix a ball's bounds independently of the trained input chart.
- `distance(X, Y)`: one finite nonnegative intrinsic distance per paired row.
- `geodesic(X, Y, s)`: paired endpoint matrices and a scalar or rowwise
  parameter in [0, 1], returning canonical rows. Endpoint values must agree
  with X/Y. The space chooses its branch or rejects ambiguous endpoints.
- `act(g, X)`: an isometry applied in canonical coordinates before the
  display map. Canonical dimension is independent of packet dimension.
- `chart_is_projective(model)`: TRUE promises straight geodesic images and
  planar faces. A nonlinear frame still requires subdivision.

`Coord3D$munch()` dispatches through the context bound by `Layout3D`.
`packet_from_primitives_3d()` calls it after primitive topology is known and
before packing. Registered emitters that construct packets directly share
the same sampler through `space_packet_3d()`. Existing rigid geometry marked
as model-space geometry keeps its explicit placement.

`space_packet_coordinates_3d()` is the sole adapter from space output into
packet coordinates. It checks matrix shapes, normalizes with model bounds
and keeps homogeneous w. Frame mapping follows the adapter. This also handles
coordinate projection helpers and point/text anchors.

## Subdivision and rendering

The common frame sampler now accepts geodesic interpolation and a drawing
metric. Canonical lexicographic edge ordering is dimension-independent.
Subdivision samples quarter, midpoint and three-quarter positions against
the screen chord segment. Binary levels keep reversed/shared edges
bit-identical. A maximum-depth failure is explicit.

Faces use the existing triangular lattice and nested geodesic interpolation.
Each subtriangle's projected barycentre is checked against its planar
interpolant. Boundary lattice points snap to the canonical edge's own
subdivision so adjacent faces and strokes have matching geometry. Half the
requested tolerance is allocated to edge approximation and half to interior
approximation. These are sampled error checks, not interval-certified bounds
for arbitrary user-supplied curves.

Numeric paint attributes interpolate by geodesic parameter (barycentric
weights on faces); colour channels interpolate in sRGB. Primitive paint
remains piecewise constant. Discrete attributes and source
identities follow the first corner. Input-chart polygon triangulation retains
the existing planar rings and holes contract. Canonical surface generators
can supply triangles directly.

Space packets retain their emission recipe. At draw time the fitted camera
and actual viewport are used to resample, before vector, raster, hybrid or
path-tracing backend selection. Widget/export preparation uses the same
resolver. Panes, guides and frame furniture retain the frame's geometry.
Specialized spaces can supply their own frame and guides in later coordinates.
Perspective error evaluation requires sampled points in front of the eye;
chart singularities and ambiguous geodesics must be handled by the space.

## Homogeneous output

`from_canonical()` may return four homogeneous columns (x, y, z, w), w >= 0.
`space_packet_coordinates_3d()` normalises them with
`normalise_homogeneous_3d()` and the packet keeps w: the primitive builder
reads it from the internal `.w` column, affine frames map homogeneous rows
without division (`frame_map_homogeneous_3d()`), and `munch_packet_3d()`
keeps homogeneous rows. Projective models return (X, X0) for Klein, (X,
X0 + 1) for Poincare and (X1:3, 1 - X4) for stereographic S^3, so box
clipping is the engine's homogeneous clip. Subdivision measures screen error
after the camera pre-transform and before division
(`screen_positions_homogeneous_3d()`); rows at or behind infinity count as
infinite error. Steps that need Euclidean model points use
`space_model_xyz_3d()` or `packet_model_points_3d()`.

Ideal points of H^3 are canonical future null vectors; Klein and Poincare
inputs accept boundary points. Geodesics with an ideal endpoint follow the
projective line. A space whose image of an edge passes through infinity
emits the ideal point (w = 0) as an explicit corner.

## Validation

`test-integration-spaces.R` and `helper-space.R` supply a unit sphere fixture
using great circles, embedded and longitude/latitude charts. Tests cover
intrinsic distance, slerp, reverse-edge equality, screen chord error, depth
budget errors, attributes, missing rows, paths/segments, filled spherical
triangles, shared boundaries, draw-time camera changes, four-dimensional
canonical points, projective models, custom packets, and homogeneous output (`test-integration-homogeneous-spaces.R`).
Vector and raster rendering use ragg PNG devices.

The audit in this lane compared serialized Euclidean path, point and segment
packets against the starting commit `61538c6`; all were byte-identical.
The existing frame tests are also retained. Rendered spherical triangle PNGs
were inspected for both backends.

## Design sources

- ggplot2 `coord_munch`: grouping, closed paths, pre-transform subdivision.
  https://github.com/tidyverse/ggplot2/blob/main/R/coord-munch.R
- ggplot2 spatial coordinates: coordinate-owned geometry transformation and
  segmentization. https://github.com/tidyverse/ggplot2/blob/main/R/geom-sf.R
- Geomview's space/model split and conformal tessellation.
  https://geomview.sourceforge.io/docs/html/Non_002dEuclidean-Geometry.html
- geomstats' canonical representations, metrics and model conversions.
  https://geomstats.github.io/_modules/geomstats/geometry/poincare_ball.html
- Manifolds.jl's separation of manifolds, representations and charts, plus
  sphere geodesics. https://juliamanifolds.github.io/Manifolds.jl/stable/manifolds/sphere/
- SageMath's space/model interface, chart conversions and isometries.
  https://doc.sagemath.org/html/en/reference/hyperbolic_geometry/sage/geometry/hyperbolic_space/hyperbolic_interface.html

Downstream interfaces considered: #14/#15 (hyperboloid models and observer
isometries), #18 (globe charts), #19 (four-dimensional spherical points),
#20 (higher-dimensional projections), #21/#22 (canonical generated geometry
and network edges), #23 (surface triangles), #24 (sphere charts and maps),
and #25 (rapidity-space isometries). Their dedicated layers and spaces are
separate implementations.

## Finite clipping and style hooks

Spaces may implement `clip_segment(X, Y, model, ranges)`, returning a two-column
matrix of visible geodesic-parameter intervals. A NULL method preserves the
ordinary sampler. `space_path_samples_3d()` splits intervals before model
projection, shares the adaptive sampler and retains original edge parameters
for paint interpolation. Spherical arcs solve affine-plane intersections
analytically. Adjacent visible intervals join; invisible intervals break paths.
`contains(X, model, ranges)` controls point and text anchors and defaults to
TRUE. `ranges` are the model's panel ranges, so clipping planes can be pulled
back from the panel box.

`cull(corners, model, ranges)` receives geodesic simplices as a list of
corner matrices and returns TRUE for those that certainly lie outside the
panel box; their screen error is not measured, so subdivision stops at the
box and near a projection pole. `visible(X, model, ranges)` marks canonical
points the sampler maps; others (a cap around a pole) are left unmapped and
any triangle touching them is dropped. Stereographic S^3 culls a simplex when
its bounding cap lies outside one box plane of R^4 or inside the pole cap
whose image lies outside twice the box circumradius.

`style_packet(packet, context)` defaults to identity. It can attach intrinsic
normal fields or model-specific colour calculations after model placement.
Klein faces use spacelike Lorentz plane normals parallel transported to the
origin. These hooks preserve the scene schema and all packet coordinates
continue through `space_packet_coordinates_3d()`.

Projective models may implement `clip_polygon(X, model, ranges)`, returning canonical
points and weights on the original corners. A NULL result preserves ordinary
face sampling. The gnomonic implementation clips against homogeneous cone
planes, retaining great-circle edges and planar model faces. This finite
positive-hemisphere path uses unit packet weights. Intrinsic face-normal
transport shares one projective plane/centroid extractor for H^3 and S^3.
