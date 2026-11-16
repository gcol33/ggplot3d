# Package index

## Coordinates and camera views

Set the plotting box, projection and viewing direction. See [Choosing
camera views](https://gillescolling.com/ggplot3d/articles/camera.md).

- [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md)
  : Exact three-dimensional panel coordinates
- [`camera_3d()`](https://gillescolling.com/ggplot3d/reference/camera_3d.md)
  : Named camera views
- [`facet_views_3d()`](https://gillescolling.com/ggplot3d/reference/facet_views_3d.md)
  : Show one 3D scene from several views
- [`stereo_3d()`](https://gillescolling.com/ggplot3d/reference/stereo_3d.md)
  : Stereo output for 3D panels

## Angles, geographic coordinates and compositions

- [`coord_cylindrical_3d()`](https://gillescolling.com/ggplot3d/reference/coord_cylindrical_3d.md)
  : Cylindrical three-dimensional coordinates
- [`coord_spherical_3d()`](https://gillescolling.com/ggplot3d/reference/coord_spherical_3d.md)
  : Spherical three-dimensional coordinates
- [`coord_globe_3d()`](https://gillescolling.com/ggplot3d/reference/coord_globe_3d.md)
  : Longitude, latitude and height on a globe
- [`coord_ternary_3d()`](https://gillescolling.com/ggplot3d/reference/coord_ternary_3d.md)
  : Ternary prism coordinates
- [`coord_simplex_3d()`](https://gillescolling.com/ggplot3d/reference/coord_simplex_3d.md)
  : Quaternary simplex coordinates

## Height scales

- [`scale_z_continuous()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md)
  [`scale_z_discrete()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md)
  [`scale_z_log10()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md)
  [`scale_z_sqrt()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md)
  [`scale_z_reverse()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md)
  [`scale_z_date()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md)
  [`scale_z_datetime()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md)
  [`scale_z_time()`](https://gillescolling.com/ggplot3d/reference/scale_z_continuous.md)
  : Continuous z position scales
- [`zlim()`](https://gillescolling.com/ggplot3d/reference/zlim.md) : Set
  z scale limits

## Points, paths, polygons and labels

- [`geom_point_3d()`](https://gillescolling.com/ggplot3d/reference/geom_point_3d.md)
  [`geom_path_3d()`](https://gillescolling.com/ggplot3d/reference/geom_point_3d.md)
  [`geom_segment_3d()`](https://gillescolling.com/ggplot3d/reference/geom_point_3d.md)
  [`geom_text_3d()`](https://gillescolling.com/ggplot3d/reference/geom_point_3d.md)
  [`geom_polygon_3d()`](https://gillescolling.com/ggplot3d/reference/geom_point_3d.md)
  : Point, path, segment, text and polygon layers in three dimensions
- [`geom_label_3d()`](https://gillescolling.com/ggplot3d/reference/geom_label_3d.md)
  [`geom_text_repel_3d()`](https://gillescolling.com/ggplot3d/reference/geom_label_3d.md)
  [`geom_label_repel_3d()`](https://gillescolling.com/ggplot3d/reference/geom_label_3d.md)
  : Labels with boxes and labels that repel each other
- [`geom_band_3d()`](https://gillescolling.com/ggplot3d/reference/geom_band_3d.md)
  : Draw a three-dimensional ribbon or wall
- [`geom_area_3d()`](https://gillescolling.com/ggplot3d/reference/geom_area_3d.md)
  [`geom_waterfall_3d()`](https://gillescolling.com/ggplot3d/reference/geom_area_3d.md)
  : Filled area walls and waterfall curtains
- [`geom_dropline_3d()`](https://gillescolling.com/ggplot3d/reference/geom_dropline_3d.md)
  : Drop segments from points to a box face
- [`geom_rug_3d()`](https://gillescolling.com/ggplot3d/reference/geom_rug_3d.md)
  : Rug ticks on the walls of the panel box

## Reference geometry and clipping

- [`annotate_3d()`](https://gillescolling.com/ggplot3d/reference/annotate_3d.md)
  : Annotations with a height
- [`geom_plane_3d()`](https://gillescolling.com/ggplot3d/reference/geom_plane_3d.md)
  : Draw reference planes through a 3D panel
- [`geom_refline_3d()`](https://gillescolling.com/ggplot3d/reference/geom_refline_3d.md)
  [`geom_abline_3d()`](https://gillescolling.com/ggplot3d/reference/geom_refline_3d.md)
  : Reference lines through the panel box
- [`geom_bracket_3d()`](https://gillescolling.com/ggplot3d/reference/geom_bracket_3d.md)
  : Brackets and dimension lines between two points
- [`clip_plane_3d()`](https://gillescolling.com/ggplot3d/reference/clip_plane_3d.md)
  : Define clipping planes for layers and panels

## Intervals and summaries

Draw supplied bounds or summarise repeated observations. See [Displaying
intervals and
summaries](https://gillescolling.com/ggplot3d/articles/intervals.md).

- [`geom_errorbar_3d()`](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md)
  [`geom_linerange_3d()`](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md)
  [`geom_pointrange_3d()`](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md)
  [`geom_crossbar_3d()`](https://gillescolling.com/ggplot3d/reference/geom_errorbar_3d.md)
  : Intervals along x, y and z: error bars, line ranges, point ranges
  and crossbars
- [`geom_stem_3d()`](https://gillescolling.com/ggplot3d/reference/geom_stem_3d.md)
  : Stems from a base value to each point
- [`stat_summary_3d()`](https://gillescolling.com/ggplot3d/reference/stat_summary_3d.md)
  : Summarise one axis at each position on the other two

## Columns, bins and voxels

- [`geom_col_3d()`](https://gillescolling.com/ggplot3d/reference/geom_col_3d.md)
  [`geom_bar_3d()`](https://gillescolling.com/ggplot3d/reference/geom_col_3d.md)
  [`stat_count_3d()`](https://gillescolling.com/ggplot3d/reference/geom_col_3d.md)
  : Columns and counts on an x/y lattice
- [`stat_bin_2d_3d()`](https://gillescolling.com/ggplot3d/reference/stat_bin_2d_3d.md)
  [`geom_histogram_3d()`](https://gillescolling.com/ggplot3d/reference/stat_bin_2d_3d.md)
  [`stat_summary_2d_3d()`](https://gillescolling.com/ggplot3d/reference/stat_bin_2d_3d.md)
  : Two-dimensional bins drawn as columns
- [`geom_hex_3d()`](https://gillescolling.com/ggplot3d/reference/geom_hex_3d.md)
  [`stat_bin_hex_3d()`](https://gillescolling.com/ggplot3d/reference/geom_hex_3d.md)
  [`stat_summary_hex_3d()`](https://gillescolling.com/ggplot3d/reference/geom_hex_3d.md)
  : Hexagonal bins drawn as prisms
- [`geom_extrude_3d()`](https://gillescolling.com/ggplot3d/reference/geom_extrude_3d.md)
  : Extrude polygon rings along z
- [`geom_voxel_3d()`](https://gillescolling.com/ggplot3d/reference/geom_voxel_3d.md)
  : Draw voxel cells on a three-dimensional lattice
- [`stat_bin_3d()`](https://gillescolling.com/ggplot3d/reference/stat_bin_3d.md)
  : Three-dimensional bin counts

## Surfaces, meshes and point clouds

Construct surfaces, read meshes and point clouds, and measure along mesh
surfaces. See [Reading and measuring
meshes](https://gillescolling.com/ggplot3d/articles/meshes.md) and
[Reading 3D scans and point
clouds](https://gillescolling.com/ggplot3d/articles/scans.md).

- [`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md)
  [`stat_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md)
  : Triangulated surfaces in three dimensions
- [`geom_contour_3d()`](https://gillescolling.com/ggplot3d/reference/geom_contour_3d.md)
  [`geom_contour_filled_3d()`](https://gillescolling.com/ggplot3d/reference/geom_contour_3d.md)
  : Contours following a triangulated surface or a horizontal plane
- [`geom_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/geom_mesh_3d.md)
  : Draw an external triangle or polygon mesh
- [`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md)
  : Fortify an indexed three-dimensional mesh
- [`read_mesh_3d()`](https://gillescolling.com/ggplot3d/reference/read_mesh_3d.md)
  : Read a triangle or polygon mesh file
- [`read_points_3d()`](https://gillescolling.com/ggplot3d/reference/read_points_3d.md)
  : Read a point cloud file
- [`geom_splat_3d()`](https://gillescolling.com/ggplot3d/reference/geom_splat_3d.md)
  : Gaussian splats in three dimensions
- [`mesh_normals()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md)
  [`mesh_curvature()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md)
  [`mesh_subdivide()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md)
  [`mesh_smooth()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md)
  [`mesh_decimate()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md)
  [`mesh_remesh()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md)
  : Smooth, refine and simplify triangle meshes
- [`mesh_geodesic_distance()`](https://gillescolling.com/ggplot3d/reference/mesh_geodesic.md)
  [`mesh_logmap()`](https://gillescolling.com/ggplot3d/reference/mesh_geodesic.md)
  [`mesh_geodesic_path()`](https://gillescolling.com/ggplot3d/reference/mesh_geodesic.md)
  : Measure distances and paths along mesh surfaces
- [`reexports`](https://gillescolling.com/ggplot3d/reference/reexports.md)
  [`fortify`](https://gillescolling.com/ggplot3d/reference/reexports.md)
  : Objects exported from other packages
- [`stat_parametric_geodesic_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_geodesic_3d.md)
  : Geodesics on parametric surfaces
- [`stat_geodesic_distance_3d()`](https://gillescolling.com/ggplot3d/reference/stat_geodesic_distance_3d.md)
  [`stat_logmap_3d()`](https://gillescolling.com/ggplot3d/reference/stat_geodesic_distance_3d.md)
  [`stat_geodesic_path_3d()`](https://gillescolling.com/ggplot3d/reference/stat_geodesic_distance_3d.md)
  : Geodesic distance, log map and shortest paths on a mesh

## Function surfaces and fitted models

- [`stat_function_3d()`](https://gillescolling.com/ggplot3d/reference/stat_function_3d.md)
  [`geom_function_3d()`](https://gillescolling.com/ggplot3d/reference/stat_function_3d.md)
  : Sample a bivariate function in three dimensions
- [`stat_smooth_3d()`](https://gillescolling.com/ggplot3d/reference/stat_smooth_3d.md)
  [`geom_smooth_3d()`](https://gillescolling.com/ggplot3d/reference/stat_smooth_3d.md)
  : Fit a bivariate response surface
- [`stat_residual_3d()`](https://gillescolling.com/ggplot3d/reference/stat_residual_3d.md)
  [`geom_residual_3d()`](https://gillescolling.com/ggplot3d/reference/stat_residual_3d.md)
  : Residuals from a fitted surface

## Distributions, densities and hulls

- [`geom_boxplot_3d()`](https://gillescolling.com/ggplot3d/reference/geom_boxplot_3d.md)
  [`geom_violin_3d()`](https://gillescolling.com/ggplot3d/reference/geom_boxplot_3d.md)
  : Boxplots and violins standing on an x/y plane
- [`stat_density_2d_3d()`](https://gillescolling.com/ggplot3d/reference/stat_density_2d_3d.md)
  : Bivariate kernel density as a height surface
- [`stat_ridgeline_3d()`](https://gillescolling.com/ggplot3d/reference/stat_ridgeline_3d.md)
  [`geom_ridgeline_3d()`](https://gillescolling.com/ggplot3d/reference/stat_ridgeline_3d.md)
  : Grouped one-dimensional density ridges
- [`stat_density_3d()`](https://gillescolling.com/ggplot3d/reference/stat_density_3d.md)
  [`geom_isodensity_3d()`](https://gillescolling.com/ggplot3d/reference/stat_density_3d.md)
  : Trivariate density isosurfaces
- [`stat_ellipsoid_3d()`](https://gillescolling.com/ggplot3d/reference/stat_ellipsoid_3d.md)
  : Covariance and confidence ellipsoids
- [`stat_hull_3d()`](https://gillescolling.com/ggplot3d/reference/stat_hull_3d.md)
  [`geom_hull_3d()`](https://gillescolling.com/ggplot3d/reference/stat_hull_3d.md)
  : Convex hulls in three dimensions
- [`stat_alpha_shape_3d()`](https://gillescolling.com/ggplot3d/reference/stat_alpha_shape_3d.md)
  [`geom_alpha_shape_3d()`](https://gillescolling.com/ggplot3d/reference/stat_alpha_shape_3d.md)
  : Alpha shapes of point clouds
- [`biplot_3d()`](https://gillescolling.com/ggplot3d/reference/biplot_3d.md)
  : Compose a three-axis ordination biplot

## Scalar fields, slices and implicit surfaces

Extract level surfaces or inspect field values on planes. See [Slicing
fields and point
densities](https://gillescolling.com/ggplot3d/articles/field-slices.md).

- [`stat_isosurface_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isosurface_3d.md)
  [`geom_isosurface_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isosurface_3d.md)
  : Level surfaces of a gridded scalar field
- [`stat_isofunction_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isofunction_3d.md)
  [`geom_isofunction_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isofunction_3d.md)
  : Level surfaces of a trivariate function
- [`geom_volume_3d()`](https://gillescolling.com/ggplot3d/reference/geom_volume_3d.md)
  : Ray-marched scalar volumes
- [`stat_slice_3d()`](https://gillescolling.com/ggplot3d/reference/stat_slice_3d.md)
  [`geom_slice_3d()`](https://gillescolling.com/ggplot3d/reference/stat_slice_3d.md)
  [`stat_density_slice_3d()`](https://gillescolling.com/ggplot3d/reference/stat_slice_3d.md)
  [`geom_density_slice_3d()`](https://gillescolling.com/ggplot3d/reference/stat_slice_3d.md)
  : Planar slices through a gridded scalar field
- [`stat_slice_contour_3d()`](https://gillescolling.com/ggplot3d/reference/stat_slice_contour_3d.md)
  [`geom_slice_contour_3d()`](https://gillescolling.com/ggplot3d/reference/stat_slice_contour_3d.md)
  : Contour lines on slices through a gridded scalar field
- [`stat_isocurve_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isocurve_3d.md)
  [`geom_isocurve_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isocurve_3d.md)
  : Intersection curves of two implicit surfaces
- [`stat_implicit_fit_3d()`](https://gillescolling.com/ggplot3d/reference/stat_implicit_fit_3d.md)
  : Implicit surfaces fitted to point clouds
- [`sdf_sphere()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md)
  [`sdf_box()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md)
  [`sdf_torus()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md)
  [`sdf_capsule()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md)
  [`sdf_plane()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md)
  [`sdf_gyroid()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md)
  [`sdf_metaballs()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md)
  : Signed distance functions
- [`sdf_blend()`](https://gillescolling.com/ggplot3d/reference/sdf_blend.md)
  [`sdf_offset()`](https://gillescolling.com/ggplot3d/reference/sdf_blend.md)
  [`sdf_shell()`](https://gillescolling.com/ggplot3d/reference/sdf_blend.md)
  [`sdf_transform()`](https://gillescolling.com/ggplot3d/reference/sdf_blend.md)
  [`Ops(`*`<ggplot3d_sdf>`*`)`](https://gillescolling.com/ggplot3d/reference/sdf_blend.md)
  : Combine and transform signed distance functions

## Solid glyphs, parametric surfaces and sweeps

- [`geom_sphere_3d()`](https://gillescolling.com/ggplot3d/reference/geom_sphere_3d.md)
  : Draw spheres at observed positions
- [`geom_ellipsoid_3d()`](https://gillescolling.com/ggplot3d/reference/geom_ellipsoid_3d.md)
  : Ellipsoids
- [`geom_tensor_3d()`](https://gillescolling.com/ggplot3d/reference/geom_tensor_3d.md)
  : Symmetric tensor glyphs
- [`geom_glyph_3d()`](https://gillescolling.com/ggplot3d/reference/geom_glyph_3d.md)
  : One solid glyph per observation
- [`stat_parametric_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_3d.md)
  [`geom_parametric_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_3d.md)
  : Parametric surfaces
- [`geom_tube_3d()`](https://gillescolling.com/ggplot3d/reference/geom_tube_3d.md)
  [`geom_ribbon_3d()`](https://gillescolling.com/ggplot3d/reference/geom_tube_3d.md)
  : Tubes and ribbons along curves
- [`stat_sweep_3d()`](https://gillescolling.com/ggplot3d/reference/stat_sweep_3d.md)
  [`geom_sweep_3d()`](https://gillescolling.com/ggplot3d/reference/stat_sweep_3d.md)
  : Sweep a profile along a curve
- [`stat_revolve_3d()`](https://gillescolling.com/ggplot3d/reference/stat_revolve_3d.md)
  [`geom_revolve_3d()`](https://gillescolling.com/ggplot3d/reference/stat_revolve_3d.md)
  : Surfaces of revolution
- [`stat_spline_3d()`](https://gillescolling.com/ggplot3d/reference/stat_spline_3d.md)
  : Smooth curves through points

## Vector fields and streamlines

- [`geom_quiver_3d()`](https://gillescolling.com/ggplot3d/reference/geom_quiver_3d.md)
  [`geom_cone_3d()`](https://gillescolling.com/ggplot3d/reference/geom_quiver_3d.md)
  : Vector fields with solid cone heads
- [`stat_streamline_3d()`](https://gillescolling.com/ggplot3d/reference/stat_streamline_3d.md)
  : Integrate streamlines and streamtubes through a vector field

## Networks, trees and molecules

- [`geom_edge_3d()`](https://gillescolling.com/ggplot3d/reference/geom_edge_3d.md)
  [`geom_node_3d()`](https://gillescolling.com/ggplot3d/reference/geom_edge_3d.md)
  : Network edges and nodes
- [`fortify(`*`<igraph>`*`)`](https://gillescolling.com/ggplot3d/reference/fortify.igraph.md)
  : Fortify a graph for three-dimensional plotting
- [`get_edges_3d()`](https://gillescolling.com/ggplot3d/reference/get_edges_3d.md)
  : Edge table of a fortified graph
- [`stat_layers_3d()`](https://gillescolling.com/ggplot3d/reference/stat_layers_3d.md)
  : Multilayer networks
- [`stat_dendrogram_3d()`](https://gillescolling.com/ggplot3d/reference/stat_dendrogram_3d.md)
  : Dendrograms on a plane
- [`fortify(`*`<pdb>`*`)`](https://gillescolling.com/ggplot3d/reference/fortify.pdb.md)
  : Fortify molecular atoms and bonds

## Spatial data and map annotations

Plot spatial objects and scale bars. For north arrows, see [orientation
guides](https://gillescolling.com/ggplot3d/reference/guide_triad_3d.md).

- [`geom_spatraster_3d()`](https://gillescolling.com/ggplot3d/reference/geom_spatraster_3d.md)
  : Rasters as height surfaces
- [`geom_sf_3d()`](https://gillescolling.com/ggplot3d/reference/geom_sf_3d.md)
  : Simple features in three dimensions
- [`stat_profile_3d()`](https://gillescolling.com/ggplot3d/reference/stat_profile_3d.md)
  : Cross-sections of a surface along a line
- [`geom_arc_3d()`](https://gillescolling.com/ggplot3d/reference/geom_arc_3d.md)
  : Flow arcs between two points
- [`annotation_scale_3d()`](https://gillescolling.com/ggplot3d/reference/annotation_scale_3d.md)
  : A scale bar lying in a coordinate plane

## Images and plots on planes

- [`annotation_plot_3d()`](https://gillescolling.com/ggplot3d/reference/annotation_plot_3d.md)
  : A two-dimensional plot on a plane of the panel box
- [`facet_slices_3d()`](https://gillescolling.com/ggplot3d/reference/facet_slices_3d.md)
  : Small multiples on parallel planes
- [`texture_3d()`](https://gillescolling.com/ggplot3d/reference/texture_3d.md)
  : Prepare images for surface textures
- [`annotation_raster_3d()`](https://gillescolling.com/ggplot3d/reference/annotation_raster_3d.md)
  : An image on a plane of the panel
- [`geom_raster_3d()`](https://gillescolling.com/ggplot3d/reference/geom_raster_3d.md)
  : Rasters on planes

## Position adjustments

- [`position_nudge_3d()`](https://gillescolling.com/ggplot3d/reference/position_nudge_3d.md)
  : Nudge all three position families
- [`position_jitter_3d()`](https://gillescolling.com/ggplot3d/reference/position_jitter_3d.md)
  : Jitter points in three dimensions
- [`position_dodge_3d()`](https://gillescolling.com/ggplot3d/reference/position_dodge_3d.md)
  : Dodge groups side by side within a cell
- [`position_project_3d()`](https://gillescolling.com/ggplot3d/reference/position_project_3d.md)
  : Project a layer onto the walls of the panel box
- [`position_stack_3d()`](https://gillescolling.com/ggplot3d/reference/position_stack_3d.md)
  [`position_fill_3d()`](https://gillescolling.com/ggplot3d/reference/position_stack_3d.md)
  : Stack or fill intervals along an axis

## Axes, legends and orientation

- [`guide_axis_3d()`](https://gillescolling.com/ggplot3d/reference/guide_axis_3d.md)
  : Position guides for three-dimensional axes
- [`guide_triad_3d()`](https://gillescolling.com/ggplot3d/reference/guide_triad_3d.md)
  [`annotation_north_3d()`](https://gillescolling.com/ggplot3d/reference/guide_triad_3d.md)
  : Camera-aligned orientation guides
- [`guide_colourbar_3d()`](https://gillescolling.com/ggplot3d/reference/guide_colourbar_3d.md)
  [`guide_coloursteps_3d()`](https://gillescolling.com/ggplot3d/reference/guide_colourbar_3d.md)
  [`guide_legend_3d()`](https://gillescolling.com/ggplot3d/reference/guide_colourbar_3d.md)
  [`guide_bins_3d()`](https://gillescolling.com/ggplot3d/reference/guide_colourbar_3d.md)
  : Colour guides that show the scene's shading

## Lighting and surface appearance

- [`light_3d()`](https://gillescolling.com/ggplot3d/reference/light_3d.md)
  : Lighting for a 3D scene
- [`light_environment()`](https://gillescolling.com/ggplot3d/reference/light_directional.md)
  [`light_directional()`](https://gillescolling.com/ggplot3d/reference/light_directional.md)
  [`light_point()`](https://gillescolling.com/ggplot3d/reference/light_directional.md)
  [`light_head()`](https://gillescolling.com/ggplot3d/reference/light_directional.md)
  [`light_hemisphere()`](https://gillescolling.com/ggplot3d/reference/light_directional.md)
  : Lights for a 3D scene
- [`material_3d()`](https://gillescolling.com/ggplot3d/reference/material_3d.md)
  : Surface materials
- [`shadow_3d()`](https://gillescolling.com/ggplot3d/reference/shadow_3d.md)
  : Planar cast shadows
- [`depth_cue_3d()`](https://gillescolling.com/ggplot3d/reference/depth_cue_3d.md)
  : Depth cues: fog, line width and eye-dome lighting
- [`outline_3d()`](https://gillescolling.com/ggplot3d/reference/outline_3d.md)
  : Feature lines of surfaces and meshes
- [`edges_3d()`](https://gillescolling.com/ggplot3d/reference/edges_3d.md)
  : Mesh lines over surfaces
- [`hatch_3d()`](https://gillescolling.com/ggplot3d/reference/hatch_3d.md)
  : Hatching of surfaces and meshes

## Rendering and accessible descriptions

Choose scene rendering and describe the resulting figure. See [Embedding
plots in
reports](https://gillescolling.com/ggplot3d/articles/documents.md).

- [`backend_3d()`](https://gillescolling.com/ggplot3d/reference/backend_3d.md)
  : Select a 3D rendering backend
- [`get_alt_text_3d`](https://gillescolling.com/ggplot3d/reference/get_alt_text_3d.md)
  : Describe a three-dimensional plot for a document

## Interactive viewing and Shiny

- [`view_3d()`](https://gillescolling.com/ggplot3d/reference/view_3d.md)
  : Explore a plot with orbit controls, tooltips and selection
- [`view3dOutput()`](https://gillescolling.com/ggplot3d/reference/view_3d-shiny.md)
  [`renderView3d()`](https://gillescolling.com/ggplot3d/reference/view_3d-shiny.md)
  : Shiny bindings for the interactive 3D viewer

## Animation and camera paths

- [`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md)
  : Render a camera path or data that change over time
- [`tween_3d()`](https://gillescolling.com/ggplot3d/reference/tween_3d.md)
  : Interpolate built layer data between animation states
- [`camera_path_3d()`](https://gillescolling.com/ggplot3d/reference/camera_path_3d.md)
  [`orbit_3d()`](https://gillescolling.com/ggplot3d/reference/camera_path_3d.md)
  [`fly_through_3d()`](https://gillescolling.com/ggplot3d/reference/camera_path_3d.md)
  [`dolly_zoom_3d()`](https://gillescolling.com/ggplot3d/reference/camera_path_3d.md)
  : Camera paths through keyframes

## Export and 3D printing

- [`export_3d()`](https://gillescolling.com/ggplot3d/reference/export_3d.md)
  : Export a plot to a 3D model, vector or source-code file
- [`print_3d()`](https://gillescolling.com/ggplot3d/reference/print_3d.md)
  : Configure physical models for 3D printing
- [`as.mesh3d(`*`<ggplot3d>`*`)`](https://gillescolling.com/ggplot3d/reference/rgl-bridge-3d.md)
  [`plot3d(`*`<ggplot3d>`*`)`](https://gillescolling.com/ggplot3d/reference/rgl-bridge-3d.md)
  : Convert a 3D panel to an rgl mesh or rgl scene
- [`as_rayrender_3d()`](https://gillescolling.com/ggplot3d/reference/as_rayrender_3d.md)
  : Convert a 3D panel to a rayrender scene

## Four dimensions, tilings and honeycombs

- [`stat_project_3d()`](https://gillescolling.com/ggplot3d/reference/stat_project_3d.md)
  : Project four-dimensional and higher-dimensional data into three
  dimensions
- [`rotate_4d()`](https://gillescolling.com/ggplot3d/reference/rotate_4d.md)
  : Rotations in four dimensions
- [`grand_tour_3d()`](https://gillescolling.com/ggplot3d/reference/grand_tour_3d.md)
  : Grand tours of three-dimensional projections
- [`stat_section_4d()`](https://gillescolling.com/ggplot3d/reference/stat_section_4d.md)
  : Hyperplane sections of four-dimensional tetrahedral meshes
- [`stat_tiling_3d()`](https://gillescolling.com/ggplot3d/reference/stat_tiling_3d.md)
  [`stat_honeycomb_3d()`](https://gillescolling.com/ggplot3d/reference/stat_tiling_3d.md)
  : Regular tilings and honeycombs from Coxeter reflections

## Spherical and hyperbolic geometry

Intrinsic curved-space coordinates and layouts. Longitude and latitude
coordinates are listed under angles and geographic coordinates.

- [`coord_spherical_geometry_3d()`](https://gillescolling.com/ggplot3d/reference/coord_spherical_geometry_3d.md)
  : Spherical geometry in stereographic and gnomonic charts
- [`stat_hopf_fibre_3d()`](https://gillescolling.com/ggplot3d/reference/stat_hopf_fibre_3d.md)
  : Hopf fibres, latitude tori and their spherical base
- [`coord_hyperbolic_3d()`](https://gillescolling.com/ggplot3d/reference/coord_hyperbolic_3d.md)
  : Finite hyperbolic ball and sheet coordinates
- [`stat_tree_hyperbolic_3d()`](https://gillescolling.com/ggplot3d/reference/stat_tree_hyperbolic_3d.md)
  : Deterministic tree layouts in hyperbolic space
- [`hyperbolic_focus_3d()`](https://gillescolling.com/ggplot3d/reference/hyperbolic_focus_3d.md)
  : Focus paths for a hyperbolic embedding

## Spacetime and relativity

- [`coord_spacetime_3d()`](https://gillescolling.com/ggplot3d/reference/coord_spacetime_3d.md)
  : Finite Minkowski spacetime coordinates
- [`stat_light_cone_3d()`](https://gillescolling.com/ggplot3d/reference/stat_light_cone_3d.md)
  : Light cones and surfaces of constant spacetime interval
- [`stat_embedding_3d()`](https://gillescolling.com/ggplot3d/reference/stat_embedding_3d.md)
  : Schwarzschild spatial embedding diagram
- [`stat_geodesic_ode_3d()`](https://gillescolling.com/ggplot3d/reference/stat_geodesic_ode_3d.md)
  : Equatorial Schwarzschild spacetime geodesics
- [`lorentz_boost_3d()`](https://gillescolling.com/ggplot3d/reference/lorentz_boost_3d.md)
  : Lorentz boosts in canonical coordinates

## Complex functions, knots and immersed surfaces

- [`mobius_3d()`](https://gillescolling.com/ggplot3d/reference/mobius_3d.md)
  : Mobius transformations of complex numbers, spheres and half-space
- [`stat_riemann_sphere_3d()`](https://gillescolling.com/ggplot3d/reference/stat_riemann_sphere_3d.md)
  : Domain colouring on the Riemann sphere
- [`stat_riemann_surface_3d()`](https://gillescolling.com/ggplot3d/reference/stat_riemann_surface_3d.md)
  : Riemann surfaces parametrised by an inverse function
- [`stat_knot_3d()`](https://gillescolling.com/ggplot3d/reference/stat_knot_3d.md)
  : Torus knots and knot tubes
- [`stat_immersed_3d()`](https://gillescolling.com/ggplot3d/reference/stat_immersed_3d.md)
  : Immersed surface examples
- [`stat_self_intersection_3d()`](https://gillescolling.com/ggplot3d/reference/stat_self_intersection_3d.md)
  : Transverse intersection curves of a triangle mesh

## Thurston geometry and observer views

Place intrinsic objects and observers inside homogeneous spaces. See
[Viewing Thurston geometries from
inside](https://gillescolling.com/ggplot3d/articles/thurston-geometries.md).

- [`annotation_thurston_3d()`](https://gillescolling.com/ggplot3d/reference/annotation_thurston_3d.md)
  : Draw an intrinsic ball, tube, or half-space
- [`geodesic_3d()`](https://gillescolling.com/ggplot3d/reference/geodesic_3d.md)
  : Follow a geodesic in a Thurston geometry
- [`thurston_portal_3d()`](https://gillescolling.com/ggplot3d/reference/thurston_portal_3d.md)
  : Pair a face of a Thurston fundamental domain

## Geometry extension API

Emit geometry into a shared panel scene. See [Writing a geometry
extension](https://gillescolling.com/ggplot3d/articles/extending.md).

- [`layer_3d()`](https://gillescolling.com/ggplot3d/reference/layer_3d.md)
  : Adapt a registered ggplot2 layer to three dimensions
- [`register_geom_3d()`](https://gillescolling.com/ggplot3d/reference/register_geom_3d.md)
  : Register a geometry emitter
- [`new_scene_packet()`](https://gillescolling.com/ggplot3d/reference/new_scene_packet.md)
  : Construct a 3D geometry packet
- [`normalise_homogeneous_3d()`](https://gillescolling.com/ggplot3d/reference/normalise_homogeneous_3d.md)
  : Normalise homogeneous positions to the panel box

## Package

- [`ggplot3d`](https://gillescolling.com/ggplot3d/reference/ggplot3d-package.md)
  [`ggplot3d-package`](https://gillescolling.com/ggplot3d/reference/ggplot3d-package.md)
  : 3D Plots in the Grammar of Graphics
