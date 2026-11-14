# Mesh processing

Verbs on a
[`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md)
corner table (also an rgl `mesh3d` or any data frame with x, y, z,
`.face`, `.corner` and `.vertex`, optionally split into parts by
`.mesh`). They work on one half-edge representation of each part:
polygon faces are triangulated (quads along the shorter diagonal), faces
with a repeated corner are dropped, faces are wound consistently within
each connected component (with a warning when some had to be reversed),
and a vertex where several fans of faces meet only at a point is split
into one vertex per fan. An edge shared by more than two faces, or a
non-orientable surface, is an error.

## Usage

``` r
mesh_normals(m, crease = 30)

mesh_curvature(m)

mesh_subdivide(m, n = 1L, scheme = c("loop", "midpoint"))

mesh_smooth(m, iterations = 10L, lambda = 0.5, mu = -0.53)

mesh_decimate(m, target = 0.5, max_error = Inf, boundary_weight = 100)

mesh_remesh(m, edge = NULL, iterations = 10L)
```

## Arguments

- m:

  A mesh: a
  [`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md)
  corner table or a compatible data frame.

- crease:

  Dihedral angle in degrees above which an edge splits the normals of
  its vertices.

- n:

  Number of subdivision rounds.

- scheme:

  `"loop"` or `"midpoint"`.

- iterations:

  Number of smoothing or remeshing iterations.

- lambda, mu:

  Taubin step factors, `lambda > 0 > mu`.

- target:

  Number of faces to keep, or a fraction of the triangle count.

- max_error:

  Largest root mean squared distance a collapse may cause, in the units
  of the positions.

- boundary_weight:

  Weight of the planes that hold boundary edges.

- edge:

  Target edge length; NULL uses the mean edge length.

## Value

A mesh of the class of `m`.

## Details

`mesh_normals()` sets `nx`, `ny`, `nz` per corner to the angle-weighted
mean of the face normals around the vertex (Thuermer and Wuethrich
1998), taken separately on each side of an edge whose faces meet at more
than `crease` degrees and of a boundary edge; `crease = 180` gives one
smooth normal per vertex.

`mesh_curvature()` adds `gaussian_curvature` (angle defect over the
mixed Voronoi area) and `mean_curvature` (half the norm of the cotangent
Laplacian) at every vertex (Meyer et al. 2003). Mean curvature is signed
against the vertex normal of the face winding, negative where the
surface bends away from its normal, so a sphere wound outwards has mean
curvature -1/r, as in
[`stat_parametric_3d()`](https://gillescolling.com/ggplot3d/reference/stat_parametric_3d.md).
Boundary vertices are `NA`.

`mesh_subdivide()` splits every triangle into four, `n` times. `"loop"`
places the points with Loop's (1987) masks, approximating a smooth
surface; boundary curves follow the cubic B-spline rule. `"midpoint"`
keeps the triangles' planes.

`mesh_smooth()` runs Taubin's (1995) lambda/mu smoothing: each iteration
moves every interior vertex by `lambda` and then by `mu` times the
vector to the mean of its neighbours; `mu < -lambda` stops the shrinking
of plain Laplacian smoothing. Boundary vertices stay fixed.

`mesh_decimate()` collapses edges by the quadric error metric of Garland
and Heckbert (1997) until the mesh has `target` faces (a fraction of the
triangle count when below one) or the next collapse would move the
surface by more than `max_error`. Vertex quadrics sum the planes of
their faces weighted by area; the error of a collapse is the
area-weighted root mean squared distance of the merged vertex from these
planes, at the position that minimises it (with directions of small
curvature truncated, as in Lindstrom 2000). Boundary edges add
perpendicular planes weighted by `boundary_weight` times their squared
length. A collapse is refused when it would make the surface
non-manifold or turn a face normal by more than 90 degrees.

`mesh_remesh()` makes edges close to length `edge` by the isotropic
remeshing of Botsch and Kobbelt (2004): split edges longer than 4/3
`edge`, collapse edges shorter than 4/5 `edge`, flip edges towards
valence six, relax vertices tangentially and project them back onto the
input surface. Boundary vertices are kept.

Rebuilding verbs (all but `mesh_normals()` and `mesh_curvature()`) carry
attributes over: double columns constant per vertex are interpolated,
other vertex columns follow the vertex each new vertex derives from, and
columns constant per face follow the input face each new face lies in.
When the input has normals or curvature, they are recomputed.

## References

Thuermer and Wuethrich (1998). Computing vertex normals from polygonal
facets. Journal of Graphics Tools 3, 43-46.
[doi:10.1080/10867651.1998.10487487](https://doi.org/10.1080/10867651.1998.10487487)

Meyer, Desbrun, Schroeder and Barr (2003). Discrete
differential-geometry operators for triangulated 2-manifolds.
Visualization and Mathematics III, 35-57.
[doi:10.1007/978-3-662-05105-4_2](https://doi.org/10.1007/978-3-662-05105-4_2)

Loop (1987). Smooth subdivision surfaces based on triangles. MSc thesis,
University of Utah.
<https://www.microsoft.com/en-us/research/publication/smooth-subdivision-surfaces-based-on-triangles/>

Taubin (1995). A signal processing approach to fair surface design.
SIGGRAPH '95, 351-358.
[doi:10.1145/218380.218473](https://doi.org/10.1145/218380.218473)

Garland and Heckbert (1997). Surface simplification using quadric error
metrics. SIGGRAPH '97, 209-216.
[doi:10.1145/258734.258849](https://doi.org/10.1145/258734.258849)

Lindstrom (2000). Out-of-core simplification of large polygonal models.
SIGGRAPH 2000, 259-262.
[doi:10.1145/344779.344912](https://doi.org/10.1145/344779.344912)

Botsch and Kobbelt (2004). A remeshing approach to multiresolution
modeling. Symposium on Geometry Processing, 185-192.
[doi:10.2312/SGP/SGP04/189-196](https://doi.org/10.2312/SGP/SGP04/189-196)

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 12),
                 y = seq(-1, 1, length.out = 12), z = seq(-1, 1, length.out = 12))
d$value <- with(d, x^2 + y^2 + z^2)
p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value))
sphere <- ggplot2::layer_data(p + stat_isosurface_3d(levels = 0.6) + coord_3d())
sphere <- sphere[c("x", "y", "z", ".face", ".corner", ".vertex")]
coarse <- mesh_decimate(sphere, 0.25)
smooth <- mesh_smooth(mesh_subdivide(coarse), iterations = 5)
curved <- mesh_curvature(smooth)
summary(curved$mean_curvature)
ggplot2::ggplot() +
  geom_mesh_3d(data = curved, ggplot2::aes(fill = mean_curvature)) + coord_3d()
```
