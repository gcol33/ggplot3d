# Deferred text orientation

`geom_text_3d(orientation = "plane")` shapes labels with `text_mesh_3d()` and
places them through `emit_glyph_instances_3d()`. XY/XZ/YZ planes, arbitrary
normals and camera-facing policies share `text_basis_3d()`.

`normal` and `up` are vectors in normalized panel coordinates. For a frame
Jacobian J and aspect scaling S, plane tangents map through S J. Their cross
product is proportional to (S J)^(-T) times the source normal when the local
map is invertible. The up hint is projected into that plane and normalized.
The reading direction completes the basis. Glyphs remain rigid at each anchor.
The coordinate-plane convenience mode preserves its two local frame directions.
Back-face readability reverses the reading direction before in-plane rotation.

The packet attribute `glyph_mm` retains its vertex rows, model-space centres
and millimetre offsets. Text also retains local glyph coordinates in `local`
and an `orientation` list. Its `facing` is `fixed`, `camera_plane` or
`camera_point`; `u` and `v` contain fixed model tangents, `up` contains an
optional transformed up hint, and `angle` contains degrees. Vector, raster,
hybrid and pathtrace output call `resolve_glyph_packets_3d()` with the fitted
panel camera and physical viewport before compiling visibility. Repeated
resolution starts from retained local coordinates and anchors.

Camera-plane text shares the reversed camera forward direction under
perspective. Camera-point text faces from each anchor to the finite eye.
Parallel projections use the reversed sight ray, including oblique shear.
A projected up hint with zero length uses the least aligned model axis.

Viewer payload version 4 carries `panels[].glyphs` alongside the resolved
scene. Each entry contains merged-scene vertex rows, centres, local coordinates,
orientation tangents, up hints and angles, encoded with the existing typed
column transport. The browser resolves those vertices and their face normals
for each camera frame and uploads contiguous changed ranges of the shared
face buffer. Colour, depth and picking passes consume the same geometry.
Primitive, layer, row and selection identifiers remain attached to their faces.
The compiled scene packet remains schema version 1; orientation metadata is
consumed before compilation and never enters the C++ packet contract.

References: ggcube `facing_to_normal()`, `compute_text_rotation_matrix()` and
`camera_facing()` at commit `113c8c0e119e1318072e40eebd882fb73857ee95`;
[VTK vtkFollower](https://vtk.org/doc/nightly/html/classvtkFollower.html).
