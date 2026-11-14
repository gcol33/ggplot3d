# Level surfaces of a gridded scalar field

Requires a complete rectilinear x/y/z grid with a numeric value
aesthetic. Missing field values mask their incident cells. Coordinates
are transformed before extraction.

## Usage

``` r
stat_isosurface_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  levels = NULL,
  caps = FALSE,
  method = "cubes",
  simplify = NULL,
  clip = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_isosurface_3d(...)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- levels:

  Numeric scalar-field thresholds; NULL uses pretty levels.

- caps:

  Whether to cap the sublevel region on the box faces (and on the
  clipping plane).

- method:

  Extraction method: `"cubes"`, `"tetra"`, `"dual"` or `"nets"`.

- simplify:

  NULL, or a target face count (or fraction of the extracted faces) to
  which the surface is reduced by quadric edge collapse
  ([`mesh_decimate()`](https://gillescolling.com/ggplot3d/reference/mesh_processing.md)).
  Normals are then recomputed from the decimated faces, or from `fun` in
  stat_isofunction_3d().

- clip:

  NULL, or a plane made with
  [`sdf_plane()`](https://gillescolling.com/ggplot3d/reference/sdf_sphere.md).
  The surface keeps the part on the side the plane normal points away
  from. With `caps = TRUE` the sublevel region is also closed on the
  plane, as with MATLAB's `isocaps`: each grid cell is split into the
  six tetrahedra of `method = "tetra"`, the plane cuts each in a
  triangle or quad on which the linear interpolant of the field is
  linear, and the part of that polygon below the level is filled, as on
  the box faces. Plane caps point along the plane normal. The plane is
  in transformed position coordinates.

## Details

`method = "cubes"` (the default) is marching cubes with the topology of
the trilinear interpolant in each cell, as in Marching Cubes 33: faces
whose corners alternate in sign are resolved by the asymptotic decider,
and a bilinear slice test along each axis decides whether two boundary
curves are joined by a tunnel through the cell. The cell triangulation
is derived from these boundary curves rather than from a case table;
curves of seven or more crossings are fanned around their mean point.
`method = "tetra"` splits each cell into six tetrahedra around a fixed
body diagonal and regularises the result (Treece et al. 1999): crossings
closest to one grid point are merged where the merge keeps a 2-manifold,
which removes most sliver triangles and the directional bias of the
split. `method = "dual"` is dual contouring (Ju et al. 2002): one vertex
per sign-changing cell placed at the minimiser of the quadratic error of
the tangent planes at its edge crossings, so creases and corners of the
level set are kept. The 3x3 normal equations are solved by an
eigen-decomposition relative to the mass point of the crossings,
eigenvalues below 1 % of the largest are truncated, and the vertex is
clamped to its cell. Each grid edge crossing gives one quad between the
four cells around it, split along the shorter diagonal. As in manifold
dual contouring (Schaefer et al. 2007) a cell carries one vertex per
level curve loop on its faces, resolved by the asymptotic decider, and a
second level-curve arc between the same two vertices gets its own
vertex, so the surface is a 2-manifold. `method = "nets"` uses the same
connectivity with each vertex at the mean of its crossings (naive
surface nets). Crossing normals for `"dual"` are interpolated
finite-difference gradients of the grid, or central differences of `fun`
in stat_isofunction_3d.

Field values within 64 machine epsilons of a threshold, relative to the
maximum absolute field value or threshold, are snapped to that
threshold. This gives shared grid vertices at numerically coincident
intersections. Vertices are welded through per-layer edge arrays and all
levels are extracted in one sweep over the grid. Finite-difference field
gradients are interpolated and normalised at vertices (nx, ny, nz).
Lighting uses the normalised mean of each face's vertex normals. caps =
TRUE closes the sublevel region on the grid box with outward faces.
Surface triangle winding points towards increasing field value.

Default levels are interior pretty(range(value), n = 10) values. The
number of levels depends on the pretty spacing. level and z_mean are
computed face attributes; .face, .corner and .vertex preserve triangle
topology. A numeric `fill` aesthetic, a second field on the same grid,
is interpolated trilinearly to the vertices, exactly on the grid edges
where the vertices lie; with `shading = "smooth"` (see
[`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md))
it stays per vertex and is interpolated across faces, otherwise each
face takes the mean of its corners.

## References

Lewiner, Lopes, Vieira and Tavares (2003). Efficient implementation of
Marching Cubes' cases with topological guarantees. Journal of Graphics
Tools 8, 1-15.
[doi:10.1080/10867651.2003.10487582](https://doi.org/10.1080/10867651.2003.10487582)

Nielson and Hamann (1991). The asymptotic decider: resolving the
ambiguity in marching cubes. IEEE Visualization '91, 83-91.
[doi:10.1109/VISUAL.1991.175782](https://doi.org/10.1109/VISUAL.1991.175782)

Treece, Prager and Gee (1999). Regularised marching tetrahedra: improved
iso-surface extraction. Computers & Graphics 23, 583-598.
[doi:10.1016/S0097-8493(99)00076-X](https://doi.org/10.1016/S0097-8493%2899%2900076-X)

Doi and Koide (1991), An Efficient Method of Triangulating Equi-Valued
Surfaces by Using Tetrahedral Cells. IEICE E74-D, 214-224.
<https://globals.ieice.org/en_transactions/information/10.1587/e74-d_1_214/_p>

Ju, Losasso, Schaefer and Warren (2002). Dual contouring of Hermite
data. ACM Transactions on Graphics 21, 339-346.
[doi:10.1145/566654.566586](https://doi.org/10.1145/566654.566586)

Schaefer, Ju and Warren (2007). Manifold dual contouring. IEEE
Transactions on Visualization and Computer Graphics 13, 610-619.
[doi:10.1109/TVCG.2007.1012](https://doi.org/10.1109/TVCG.2007.1012)

Schroeder, Maynard and Geveci (2015). Flying edges: a high-performance
scalable isocontouring algorithm. IEEE LDAV, 33-40.
[doi:10.1109/LDAV.2015.7348069](https://doi.org/10.1109/LDAV.2015.7348069)

## Examples

``` r
d <- expand.grid(x = seq(-1, 1, length.out = 10),
                 y = seq(-1, 1, length.out = 10), z = seq(-1, 1, length.out = 10))
d$value <- with(d, x^2 + y^2 + z^2)
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
  geom_isosurface_3d(levels = 0.6) + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
  stat_isosurface_3d(levels = 0.6) + coord_3d()
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value, fill = z)) +
  stat_isosurface_3d(levels = 0.6, shading = "smooth") + coord_3d()
```
