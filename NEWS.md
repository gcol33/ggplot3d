# ggplot3d 0.0.0.9000

* `geom_tube_3d(radius.unit = "mm")`, `geom_ribbon_3d(width.unit = "mm")` and
  `stat_sweep_3d(radius.unit = "mm")` size the cross section in millimetres on
  the page. The sweep runs when the plot is drawn, in the box the camera
  sees, so tubes stay round whatever the box proportions, in the vector,
  raster and hybrid backends (#36).

* `view_3d()` honours `alpha_mode = "surface"`: a depth pass per sheet keeps
  the nearest fragment of each translucent sheet before the weighted blended
  transparency accumulates it, so a sheet folding over itself does not darken
  and nested isosurfaces show through each other (#58).

* The raster and hybrid backends honour `alpha_mode = "surface"`: each sample
  keeps the nearest fragment of every translucent sheet and composites the
  sheets front to back in linear light, so a sheet folding over itself does
  not darken and nested isosurfaces still show through each other (#58).

* Scene schema 2: packet vertices are homogeneous `x, y, z, w` (`w` defaults
  to 1; `w = 0` is a point at infinity), and the engine clips in homogeneous
  coordinates before dividing (Blinn and Newell 1978), so ideal polyhedra and
  half-space cells clip at the panel box. `normalise_homogeneous_3d()` maps
  homogeneous data positions to box fractions. Cameras take a 4 x 4 `pre`
  map applied inside the engine (boosts and projective maps without
  re-emitting). Corners take optional normals, colours and texture
  coordinates carried by the vertex weights; styles an optional `texture_id`;
  polylines a `mask_width_mm` for haloed lines (Appel 1967); and packets a
  `quadrics` table that the engine does not compile yet (#16).

* Hyperbolic and spherical coordinates accept `inside = TRUE`, finite observer
  points or isometries, and optional inverse-geodesic-area depth cues. The
  vector backend views Klein or gnomonic geometry from the model origin;
  spherical faces clip to a finite observer-hemisphere box. Compile-only
  per-frame observer boosts await homogeneous camera pre-transforms (#15).

* `coord_spherical_geometry_3d()` supplies S^3 stereographic and gnomonic
  charts with great-circle munching and SO(4) actions. `stat_hopf_fibre_3d()`
  draws fibres as paths or tubes, latitude tori including the Clifford torus,
  and matching base-sphere points. Pole-crossing fibres clip analytically to
  a finite panel box before projection (#19).

* Hyperbolic coordinates default to Klein display, support negative curvature,
  observer centring, intrinsic radial distance guides and transported Minkowski
  normals for lighting. Poincare and upper half-space geodesics share adaptive
  munching; half-space output clips to a finite panel box (#14).
  Input defaults to the display model. Tree layers register their Poincare
  input independently of the selected display chart.

* Project 4D and n-D data with `stat_project_3d()`, all six SO(4) plane
  rotations and quaternion double rotations, finite perspective and adaptive
  stereographic surfaces. `tour_grand()` drives Grassmann-interpolated
  projection animations. `stat_section_4d()` shares marching-tetrahedra topology
  with isosurfaces and welds hyperplane-section vertices (#20).

* `stat_tiling_3d()` and `stat_honeycomb_3d()` generate finite spherical, Euclidean and compact hyperbolic Coxeter orbits, with edge/face deduplication and curved-model sampling. Registered layers can supply canonical space specifications and remunch against the final viewport. Ideal and hyperideal cases await homogeneous clipping or explicit truncation.

* `coord_hyperbolic_3d()` draws finite Poincare/Klein-ball networks and hyperboloid-sheet lifts through shared geodesic sampling. `stat_tree_hyperbolic_3d()` creates deterministic disk and ball layouts, and `hyperbolic_focus_3d()` supplies Lorentz focus paths for animation.

* Relativity layers draw light cones, constant-interval hyperboloids, Schwarzschild spatial embeddings and equatorial timelike/null orbits. `coord_spacetime_3d()` and `lorentz_boost_3d()` support finite Lorentz transformations, including per-frame `animate_3d(isometry = )` schedules.

* Complex-analysis layers draw Riemann-sphere phase portraits and inverse-parametrised Riemann surfaces. `mobius()` maps extended complex values, the finite Riemann sphere and hyperbolic half-space.

* `stat_self_intersection_3d()` extracts transverse triangle crossings as stitched paths. `stat_immersed_3d()` supplies Klein bottles, Boy's surface, a cross-cap and a Moebius band; `stat_knot_3d()` generates torus knots and closed tubes.

* `coord_globe_3d()` maps longitude, latitude and height to a sphere, munches great-circle paths and faces, reprojects sf inputs, and draws a graticule and opaque lit globe.

* `coord_3d()` accepts a space, input chart, display model and isometry. Its shared geodesic munch hook adaptively subdivides paths and polygon faces in projected panel coordinates; the Euclidean default preserves existing packets. Homogeneous models and ideal points await scene schema 2.

* `animate_3d(trail = )` draws fading tubes or ribbons behind moving marks, using rendered frame history and the shared rotation-minimising sweep. Trails follow tween identities, support radius or width aesthetics and optional tapering, and break at missing observations.

* `geom_text_3d(orientation = "plane")` accepts arbitrary `normal` and `up` directions and `facing = "camera_plane"` or `"camera_point"`. Font outlines resolve against each panel's drawing camera across static backends and follow camera movement in the HTML viewer, including depth-tested picking.

* Package checks store the OBJ mesh fixture as text, declare test dependencies and computed variables, document common stat arguments, respect the raster thread limit and catch the introductory vignette's expected missing-z error while drawing the plot.

* Viewers and model exporters share physical panel measurement independently of the static renderer, so point-cloud widgets bypass the vector marker limit and static visibility compilation.
* `view3dOutput()` and `renderView3d()` embed reactive viewers in Shiny applications and modules, with camera and selection inputs.

* `export_3d(print = print_3d())` produces checked, closed millimetre solids as binary STL or coloured 3MF. Printing adds physical feature sizes, surface shells or base closures, extruded labels and axes, and Boolean union through the bundled native Manifold library.

* `annotate_3d()` forwards registered drawing parameters through the shared 3D layer constructor, including bracket types, offsets and dimension arrows. Fixed spans in the Layers article use annotation calls.

* The HTML viewer uses weighted blended order-independent transparency with linear-light accumulation. Circular markers use instanced disc quads with one GPU record per point, retaining footprint rotation, borders, picking and selection.

* `view_3d(panel = )` sizes the selected facet and its axis furniture for the widget, preserving the panel aspect. knitr and Quarto round figure dimensions to whole pixels for HTML viewers.

* Domain recipes cover symmetric tensor glyphs, ordination biplots, molecular ball-and-stick views and sampled Lab colour gamuts. `geom_tensor_3d()` adds Kindlmann superquadrics through shared glyph instancing, `stat_biplot_3d()` composes score points, quivers and group regions, and `fortify.pdb()` joins atoms and bonds by serial number.

* `save_svg_3d()` writes native depth-ordered SVG with layer and row identities, tooltips and linked hover highlighting. `export_code_3d()` writes editable TikZ paths or model-space Asymptote scenes with camera and optional PRC output.

* ggplot2's `geom_contour_filled()` draws terraced bands at their level midpoints inside `coord_3d()`, preserving polygon holes and the band fill scale.

* `geom_quiver_3d()` draws vectors with solid cone heads and line or tube shafts; `geom_cone_3d()` draws magnitude-scaled cones. `stat_streamline_3d()` integrates trilinear vector fields with RK4 or adaptive RK45, accepts point/rake/plane seeds and Jobard-Lefer slice placement, and sweeps streamtubes through the shared tube machinery.

* `backend_3d("raymarch")` renders in-space views of all eight Thurston geometries with geodesic sphere tracing, paired quotient faces, parallel transport, and geodesic-sphere area lighting. `geom_thurston_3d()` supplies intrinsic primitives, and `geodesic_3d()` exposes the shared flow and transport calculations.

* `geom_boxplot_3d()` draws box prisms, median faces, whiskers and outliers on x/y cells. `geom_violin_3d()` draws mirrored density walls or revolved solids. `geom_waterfall_3d()` draws curtains and `geom_area_3d()` stacks area walls through `position_stack_3d()`.
* `backend_3d("pathtrace")` adds diffuse global illumination, distant-disk soft shadows, thin-lens depth of field and seeded progressive accumulation. Native BVH traversal shares the SDF ray kernel; batches and worker counts produce identical samples.

* knitr and Quarto chunks accept `view3d = TRUE` for the HTML viewer and static vector scenes in PDF and Word. `alt_text_3d()` supplies figure descriptions, with `fig.alt` and `labs(alt = )` overrides. Patchwork and cowplot compositions retain their separate panel scenes.
* `geom_volume_3d()` renders trilinear scalar grids with emission-absorption transfer functions, maximum-intensity projection, and per-cell cubic isosurface intersections. Overlapping media combine extinction and emission, and opaque scene depth bounds the integration.

* `animate_3d(time = , tween = tween_3d())` interpolates built layer data between states. Marks match by group or source row, paths resample by arc length, fixed meshes morph by face keys, and changing connectivity cross-fades. Direct gganimate transitions give guidance to use the 3D animation pipeline.

* `geom_isofunction_3d(render = "trace")` compiles registered SDF arithmetic to a C++ tape, sphere-traces opaque surfaces with Lipschitz bounds, and composites their analytic normals and camera depths with opaque raster geometry.

* `view_3d()` displays facet panels with linked cameras and embeds strips, legends and titles as SVG. WebGL 2 batches primitives and provides picking, tooltips, selection, Shiny inputs and crosstalk selection/filtering. Named camera link groups also support path playback.
* `export_3d()` writes glTF/GLB, OBJ, PLY and binary STL scenes with layer nodes, mapped colours and cameras. `animate_3d()` exports camera and geometry tracks to glTF. rgl mesh and plotting methods and `as_rayrender_3d()` connect scenes to other renderers.
* `geom_refline_3d()` draws axis-aligned reference lines and `geom_abline_3d()` draws lines through a point in any direction. `geom_bracket_3d()` adds brackets and dimension lines. Reference lines share box clipping and bend with curvilinear frames.
* `annotation_plot_3d()` places a whole 2D ggplot on a panel plane through vector or raster rendering. `facet_slices_3d()` draws small multiples in depth, with statistics computed per slice. Additional 2D ggplot2 layers draw in horizontal planes.
* `guide_colourbar_3d(along = "z")` places a colourbar beside the matching position axis, sharing its ticks and labels. `guide_legend_3d()` lights shaded-layer facet keys with the scene light and can display their shading range.

* Geospatial inputs: terra and stars rasters become height surfaces, simple features draw as points, paths, polygons or prisms, and lidR point clouds fortify to point attributes. `stat_profile_3d()` samples cross-sections and `geom_arc_3d()` draws flow arcs. The spatial article includes a space-time cube.
* Fitted surfaces accept fitting functions and fitted models with `predict()` methods, hull-masked domains and confidence bands, surfaces or standard-error fills. `stat_residual_3d()` and `geom_residual_3d()` draw observation-to-fit segments with computed fitted values, residuals and signs.
* `guide_triad_3d()` and `annotation_north_3d()` show camera-aligned model directions in each panel. `annotation_scale_3d()` draws a bar in data units with ticks and a label in its coordinate plane, including perspective views.

* `stat_ellipsoid_3d()` fits grouped covariance and confidence ellipsoids with t, normal or Euclidean shapes, Hotelling regions for the mean and enclosing hulls. It computes `level`, `volume` and `n`; `axes = TRUE` draws the principal axes.
* Interval layers on all three axes: `geom_errorbar_3d()`, `geom_linerange_3d()`, `geom_pointrange_3d()` and `geom_crossbar_3d()` draw any combination of x, y and z intervals. `geom_stem_3d()` draws stems from a base value; `stat_summary_3d()` summarises one axis at each position on the other two.
* Reference and clipping planes: `geom_plane_3d()` draws planes of constant x, y or z, or of any orientation from a normal and a point (such as a fitted regression plane), cut to the panel box as one face that intersects surfaces exactly. `coord_3d(clip_planes = )` cuts every layer by `clip_plane_3d()` half-spaces set in data units, and a layer's `clip_planes` argument replaces them for that layer; `cap = TRUE` closes cut solids with faces on the plane, leaving the holes of hollow solids open.
* `guide_axis_3d()` adds edge-following labels, glyph outlines lying in a pane, in-plane ticks, near-end titles and perspective depth scaling. Plane text and ticks participate in scene visibility. `geom_text_3d(orientation = "plane", plane = )` lays font outlines in a coordinate plane.

* Networks and trees: `ggplot(graph)` fortifies an igraph graph into nodes and edges with a 3D igraph layout (`fortify(graph, layout = )`); `geom_edge_3d()` draws edges as lines, arcs or shaded tubes and `geom_node_3d()` the nodes; `stat_layers_3d()` stacks the layers of a multilayer network on z planes with inter-layer edges; `stat_dendrogram_3d()` draws an `hclust` tree in any coordinate plane, such as a wall beside a floor heatmap, with branches coloured by cluster.
* Draw points, paths, segments, text, polygons, surfaces, contours, meshes, bands, columns and extrusions in three dimensions.
* Train continuous, discrete and date/time z scales, with fixed or free z ranges across facets.
* Control camera angles, projection, lighting and framing with `coord_3d()`.
* `coord_3d(axis_lines = "origin", origin = )` and `guide_axis_3d(position = "origin")` draw axes through a data point as part of the scene, with ticks, tick labels and titles that data can hide; `axis_lines = "none"` drops the box axis lines. `panes = "front"` adds translucent foreground panes (`panel.pane.front`). Theme elements `panel.grid.major.x.xy` and the like style or remove the grid of one axis on one plane, `panel.zero.x`, `.y`, `.z` draw a plane at transformed value 0 that depth-sorts with the data, and `panel.zero.line` marks 0 on the panes.
* Stereo output: `coord_3d(stereo = stereo_3d())` draws each panel for two eyes turned about the camera target by `separation` degrees, as a side-by-side pair for parallel (`"pair"`) or cross-eyed (`"cross"`) viewing, or as a red-cyan (or any two filter colours) anaglyph. Anaglyphs blend two luminance passes with a multiply group on devices that support it and otherwise composite per colour channel offscreen, as the raster backend always does; colour and fill scales then show luminance only, with a warning.
* Panel frames: `coord_cylindrical_3d()` and `coord_spherical_3d()` bend paths and faces along angle, radius and height, with rear wall or shell panes and curved axes; `coord_ternary_3d()` and `coord_simplex_3d()` place three- or four-part compositions (aesthetics `a`, `b`, `c`, `d`) in a triangular prism or a regular tetrahedron.
* `coord_3d(transform = )` applies coordinate transformations after statistics, with expansion in transformed space.
* `stat_density_3d()` and `stat_hull_3d()` gain `space = "ilr"` for compositions.
* `camera_3d()` returns named views (isometric, dimetric, trimetric, top, front, right, and the oblique cabinet, cavalier and military views); `coord_3d()` gains `projection = "oblique"`, `oblique` and `up`.
* Resolve visibility across layers within each panel for vector and raster output.
* Large scenes compile faster: a 200,000-face surface takes about 2 s instead of 7 s. The occlusion graph uses several threads, set with `options(ggplot3d.threads = )`; the result is the same for every thread count. Redrawing a plot with the same panel size and view reuses the compiled scene (`options(ggplot3d.cache_bytes = )`).
* Compute function surfaces, fitted surfaces, density surfaces, ridgelines, convex hulls and scalar-field level surfaces.
* Bin two variables into columns: `stat_bin_2d_3d()` and its bar-style form `geom_histogram_3d()` count observations in rectangles, `geom_hex_3d()` and `stat_bin_hex_3d()` in hexagons drawn as prisms, and `stat_summary_2d_3d()` and `stat_summary_hex_3d()` summarise a mapped z in each bin. Computed variables follow ggplot2 (`count`, `density`, `ncount`, `ndensity`) plus the bin `width` and `depth`.
* Animate data that change over time with `animate_3d(time = )`: one frame per value of a column, with scales trained on all frames.
* Camera paths: `camera_path_3d()` interpolates keyframed views with quaternion orientation (slerp or squad), a centripetal Catmull-Rom target and log-scaled field of view, zoom and distance, with easing per segment; `orbit_3d()`, `fly_through_3d()` and `dolly_zoom_3d()` (including a perspective-to-orthographic morph at constant image scale) build common moves, and paths join with `c()`. `animate_3d(path = )` renders them and `view_3d(path = )` plays them in the browser.
* `animate_3d()` writes WebM, animated PNG, PNG and SVG sequences and multi-page PDF besides GIF and MP4, chosen by the file extension, and draws frames in parallel with `workers`.
* `geom_blank()` and `expand_limits()` train scales inside `coord_3d()`.
* The raster backend renders 64-pixel tiles on several threads (option `ggplot3d.threads`) with one fragment list per sample, keeps the exact compositing order and can return depth, id and normal buffers.
* The raster backend composites translucent paint and averages antialiasing samples in linear light; `backend_3d(blend = "srgb")` matches vector devices.
* Surface layers take `alpha_mode = "surface"`, which paints each connected sheet of a translucent surface as one film in vector output: nested isosurfaces show through each other while a sheet folding over itself does not darken.
* `geom_point_3d(render = "splat")` draws large point clouds in the raster backend as depth-tested spheres with eye-dome lighting (`edl`); the vector backend stops above `getOption("ggplot3d.vector_markers", 50000)` markers per panel.
* `backend_3d("hybrid")` rasterises the layers drawn with `raster = TRUE` and keeps the other layers, axes, panes and text as vector graphics, clipped where the image's opaque coverage lies in front of them.
* Translucent surfaces in vector output cover the shared edges inside each group of non-overlapping faces once.
* `geom_label()`, `geom_rect()`, `geom_tile()`, `geom_hline()`, `geom_vline()`, `geom_abline()`, `annotation_custom()` and `annotation_raster()` work inside `coord_3d()`; reference lines lie on the floor unless z is mapped.
* Position adjustments in three dimensions: `position_jitter_3d()` jitters x, y and z; `position_dodge_3d()` places groups side by side along x or y within each x/y cell; `position_stack_3d()` and the new `position_fill_3d()` stack along z, x or y, for columns, walls and bands, points and text. `register_geom_3d()` gains `positions` and `extents`, so each geometry declares the adjustments it accepts.
* `annotate_3d()` adds fixed annotations with z positions.
* `position_project_3d()` draws any layer flattened onto the panes of the panel box: `"floor"`, `"back"` (chosen for the camera), a pane name, or a plane at a data value, several at once. Projected primitives are painted with their pane. With `axes = c("x", "z")` a two-dimensional ggplot2 layer, such as a histogram or `geom_density()`, stands on a wall as a marginal plot. The `plane` argument of contour and density layers is a shorthand for it.
* `geom_rug_3d(sides = c("floor:x", "back:z"))` marks positions with rug ticks on the walls.
* `geom_ribbon()`, `geom_area()` and `geom_density()` work inside `coord_3d()` as flat bands.
* `geom_label_3d()` draws boxed labels. `geom_text_repel_3d()` and `geom_label_repel_3d()` move labels off each other and off the data points on the page at draw time, after projection, and join each moved label to its point with a leader line. The C++ solver takes ggrepel's arguments (`force`, `force_pull`, `max.iter`, `max.time`, `max.overlaps`, `box.padding`, `point.padding`, `min.segment.length`, `direction`, `seed`); `animate_3d()` starts each frame's labels from the previous frame's offsets.
* `stat_density_2d_3d(plane = )` draws the density flat on the floor or at a given height, filled by density.
* The camera arguments of `coord_3d()` take one value per panel, so facets can show different views; `animate_3d()` keeps each panel's offset.
* `facet_views()` draws one scene from several cameras, one panel per view, with the layers computed once and one magnification across panels. `layout = "third_angle"` and `"first_angle"` place front, top, bottom, left, right and rear views by the projection methods of ISO 5456-2, so edges line up across adjacent views. `camera_3d()` gains the `"bottom"`, `"rear"` and `"left"` views.
* `view_3d(link = )` links viewers on one page: an orbit, zoom or reset in one moves every viewer of the group, each keeping its own view.
* Outlined faces seen edge-on, as in front, top and side views, no longer stop the compilation of hidden strokes.
* `labs(x = NULL)` and friends remove a 3D axis title.
* Discrete axes keep every label unless `check.overlap = TRUE`; tick labels are thinned by their measured rectangles, `n.dodge` rows stack directly outside one another, and axis titles sit next to the tick labels.
* Solid glyph layers: `geom_sphere_3d()` (radius in data units or on-screen millimetres), `geom_ellipsoid_3d()` (semi-axes `rx`, `ry`, `rz` with an optional `rotation`, or a `sigma` shape matrix) and `geom_glyph_3d()`, which places a sphere, cube, cone, cylinder, octahedron or any mesh at each observation, sized by `size` and turned by `u`, `v`, `w`. Glyphs are meshes in the panel scene, so they intersect other geometry, take light and keep perspective.
