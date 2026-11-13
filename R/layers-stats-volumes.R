#' Request probability-content density levels
#'
#' Converts requested enclosed probabilities into a tagged specification for
#' stat_density_3d. Grid quadrature accumulates Gaussian density from highest
#' values downwards until each absolute probability content is reached. Scale
#' limits should include the density tails; inadequate captured mass warns.
#' @param ... Probabilities strictly between zero and one.
#' @return A numeric vector tagged as a probability-content specification.
#' @export
#' @examples
#' probs(0.5, 0.9)
probs <- function(...) {
  p <- c(...)
  if (!is.numeric(p) || !length(p) || any(!is.finite(p) | p <= 0 | p >= 1)) stop("Probabilities must lie strictly between zero and one.", call. = FALSE)
  structure(as.double(p), class = "ggplot3d_probs")
}

#' Level surfaces of a gridded scalar field
#'
#' Requires a complete rectilinear x/y/z grid with a numeric value aesthetic.
#' Missing field values mask their incident cells. Coordinates are transformed
#' before extraction.
#'
#' `method = "cubes"` (the default) is marching cubes with the topology of the
#' trilinear interpolant in each cell, as in Marching Cubes 33: faces whose
#' corners alternate in sign are resolved by the asymptotic decider, and a
#' bilinear slice test along each axis decides whether two boundary curves
#' are joined by a tunnel through the cell. The cell triangulation is derived
#' from these boundary curves rather than from a case table; curves of seven
#' or more crossings are fanned around their mean point.
#' `method = "tetra"` splits each cell into six tetrahedra around a fixed body
#' diagonal and regularises the result (Treece et al. 1999): crossings closest
#' to one grid point are merged where the merge keeps a 2-manifold, which
#' removes most sliver triangles and the directional bias of the split.
#' `method = "dual"` is dual contouring (Ju et al. 2002): one vertex per
#' sign-changing cell placed at the minimiser of the quadratic error of the
#' tangent planes at its edge crossings, so creases and corners of the level
#' set are kept. The 3x3 normal equations are solved by an eigen-decomposition
#' relative to the mass point of the crossings, eigenvalues below 1 % of the
#' largest are truncated, and the vertex is clamped to its cell. Each grid edge
#' crossing gives one quad between the four cells around it, split along the
#' shorter diagonal. As in manifold dual contouring (Schaefer et al. 2007) a
#' cell carries one vertex per level curve loop on its faces, resolved by the
#' asymptotic decider, and a second level-curve arc between the same two
#' vertices gets its own vertex, so the surface is a 2-manifold.
#' `method = "nets"` uses the same connectivity with each vertex at the mean
#' of its crossings (naive surface nets).
#' Crossing normals for `"dual"` are interpolated finite-difference gradients
#' of the grid, or central differences of `fun` in stat_isofunction_3d.
#'
#' Field values within 64 machine epsilons of a threshold, relative to the
#' maximum absolute field value or threshold, are snapped to that threshold.
#' This gives shared grid vertices at numerically coincident intersections.
#' Vertices are welded through per-layer edge arrays and all levels are
#' extracted in one sweep over the grid.
#' Finite-difference field gradients are interpolated and normalised at vertices
#' (nx, ny, nz). Lighting uses the normalised mean of each face's vertex normals.
#' caps = TRUE closes the sublevel region on the grid box with outward faces.
#' Surface triangle winding points towards increasing field value.
#'
#' Default levels are interior pretty(range(value), n = 10) values. The number
#' of levels depends on the pretty spacing. level and z_mean are computed
#' face attributes; .face, .corner and .vertex preserve triangle topology.
#' @inheritParams stat_function_3d
#' @param levels Numeric scalar-field thresholds; NULL uses pretty levels.
#' @param caps Whether to cap the sublevel region on the box faces (and on
#'   the clipping plane).
#' @param clip NULL, or a plane made with [sdf_plane()]. The surface keeps the
#'   part on the side the plane normal points away from. With `caps = TRUE` the
#'   sublevel region is also closed on the plane, as with MATLAB's
#'   `isocaps`: each grid cell is split into the six tetrahedra of `method =
#'   "tetra"`, the plane cuts each in a triangle or quad on which the linear
#'   interpolant of the field is linear, and the part of that polygon below
#'   the level is filled, as on the box faces. Plane caps point along the
#'   plane normal. The plane is in transformed position coordinates.
#' @param method Extraction method: `"cubes"`, `"tetra"`, `"dual"` or `"nets"`.
#' @param simplify NULL, or a target face count (or fraction of the extracted
#'   faces) to which the surface is reduced by quadric edge collapse
#'   ([mesh_decimate()]). Normals are then recomputed from the decimated
#'   faces, or from `fun` in stat_isofunction_3d().
#' @references Lewiner, Lopes, Vieira and Tavares (2003). Efficient
#'   implementation of Marching Cubes' cases with topological guarantees.
#'   Journal of Graphics Tools 8, 1-15. \doi{10.1080/10867651.2003.10487582}
#'
#'   Nielson and Hamann (1991). The asymptotic decider: resolving the
#'   ambiguity in marching cubes. IEEE Visualization '91, 83-91.
#'   \doi{10.1109/VISUAL.1991.175782}
#'
#'   Treece, Prager and Gee (1999). Regularised marching tetrahedra: improved
#'   iso-surface extraction. Computers & Graphics 23, 583-598.
#'   \doi{10.1016/S0097-8493(99)00076-X}
#'
#'   Doi and Koide (1991), An Efficient Method of Triangulating
#'   Equi-Valued Surfaces by Using Tetrahedral Cells. IEICE E74-D, 214-224.
#'   <https://globals.ieice.org/en_transactions/information/10.1587/e74-d_1_214/_p>
#'
#'   Ju, Losasso, Schaefer and Warren (2002). Dual contouring of Hermite data.
#'   ACM Transactions on Graphics 21, 339-346. \doi{10.1145/566654.566586}
#'
#'   Schaefer, Ju and Warren (2007). Manifold dual contouring. IEEE
#'   Transactions on Visualization and Computer Graphics 13, 610-619.
#'   \doi{10.1109/TVCG.2007.1012}
#'
#'   Schroeder, Maynard and Geveci (2015). Flying edges: a high-performance
#'   scalable isocontouring algorithm. IEEE LDAV, 33-40. \doi{10.1109/LDAV.2015.7348069}
#' @export
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 10),
#'                  y = seq(-1, 1, length.out = 10), z = seq(-1, 1, length.out = 10))
#' d$value <- with(d, x^2 + y^2 + z^2)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
#'   geom_isosurface_3d(levels = 0.6) + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
#'   stat_isosurface_3d(levels = 0.6) + coord_3d()
stat_isosurface_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                                position = "identity", ..., levels = NULL, caps = FALSE,
                                method = "cubes", simplify = NULL, clip = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!is.null(clip)) plane_coefficients_3d(clip, "clip")
  make_layer_3d(geom, mapping, data, StatIsosurface3D, position,
    stats_params_3d(list(levels = levels, caps = caps, method = isosurface_method_3d(method), simplify = simplify, clip = clip, na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_isosurface_3d
#' @export
geom_isosurface_3d <- function(...) stat_isosurface_3d(...)

#' Trivariate density isosurfaces
#'
#' Evaluates an axis-aligned Gaussian product kernel on transformed x/y/z scale
#' ranges. h contains Gaussian standard deviations in transformed units.
#' Automatic bandwidths use normal-reference marginal bandwidths with sample
#' size exponent -1/7. adjust multiplies automatic bandwidths.
#' Numeric levels are density thresholds; probs() specifies absolute enclosed
#' probability contents estimated by tensor trapezoidal quadrature.
#' Computed level and density contain the resolved density threshold.
#' @inheritParams stat_isosurface_3d
#' @inheritParams stat_density_2d_3d
#' @param n Grid resolution, scalar or three integers.
#' @param space `"data"` estimates in the position scales. `"ilr"` estimates a
#'   composition mapped with `a`, `b`, `c` (and `d`) in isometric log-ratio
#'   coordinates, where a Gaussian kernel respects relative scale, and maps
#'   the surface back into the simplex. With three parts, z stays as it is.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(x = rnorm(80), y = rnorm(80), z = rnorm(80))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_isodensity_3d(h = 0.6, levels = 0.02, n = 12) + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   stat_density_3d(h = 0.6, levels = 0.02, n = 8) + coord_3d()
stat_density_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                             position = "identity", ..., h = NULL, adjust = 1, n = 32L,
                             levels = NULL, caps = FALSE, space = "data", method = "cubes", simplify = NULL, clip = NULL,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!is.null(clip)) plane_coefficients_3d(clip, "clip")
  make_layer_3d(geom, mapping, data, StatDensity3D, position,
    stats_params_3d(list(h = h, adjust = adjust, n = n, levels = levels, caps = caps, method = isosurface_method_3d(method), simplify = simplify, clip = clip,
      space = match.arg(space, c("data", "ilr")), na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_density_3d
#' @export
geom_isodensity_3d <- function(...) stat_density_3d(...)

#' Level surfaces of a trivariate function
#'
#' Evaluates fun on transformed x/y/z scale ranges and passes the scalar field
#' to the same extraction used by stat_isosurface_3d. Since fun is known
#' everywhere, each vertex on a grid edge is then moved onto fun = level by
#' `refine` Illinois regula falsi steps inside the edge whose endpoints bracket
#' the level, and vertex normals (nx, ny, nz) are central differences of fun at
#' the final vertices, with steps of 1e-5 times each axis range. Vertices that
#' lie on no grid edge (merged `"tetra"` clusters, centre vertices of
#' `"cubes"`) keep their position and take the normal from fun.
#'
#' `adaptive = TRUE` replaces the grid by an octree over the scale box
#' (Plantinga and Vegter 2004). fun is evaluated on whole boxes in interval
#' arithmetic, which encloses its range and gradient over each box. A box
#' whose range contains no level is dropped. Every other box is split down to
#' `depth[1]`, and further, up to `depth[2]`, while the inner product of its
#' gradient enclosure with itself may reach zero (the small normal variation
#' test: without it the level set inside the box can fold) or while the box
#' diagonal times r / |c| exceeds `tol` times the diagonal of the scale box,
#' where c and r are the centre and half diagonal of the gradient enclosure.
#' Every gradient in the box then lies within asin(r / |c|) of c, so `tol`
#' bounds how far the level set bends away from a plane inside a box.
#' Gradients are measured with each axis scaled to the unit interval. The
#' surface is then extracted by dual contouring on the octree (Ju et al.
#' 2002): each sign change on a minimal edge, an edge no smaller box
#' subdivides, gives one polygon through the vertices of the three or four
#' boxes around it, so boxes of different sizes join without cracks. Each box
#' carries one vertex per level-curve loop on its faces, as with `method =
#' "dual"` on a grid. Edge crossings are placed by `refine` regula falsi steps
#' and their normals come from fun. Only `method = "dual"` (the default with
#' `adaptive = TRUE`) and `"nets"` apply; `n` is not used.
#'
#' fun is evaluated on interval objects, so it may use `+`, `-`, `*`, `/`,
#' `^`, `abs()`, `sqrt()`, `exp()`, `expm1()`, `log()`, `log1p()`,
#' `log2()`, `log10()`, `sin()`, `cos()`, `sinh()`, `cosh()`, `tanh()`,
#' `atan()`, `min()`, `max()`, and `pmin()` and `pmax()` called in its own
#' body. Any other operation, including comparisons and `ifelse()`, stops
#' with an error that names the call.
#'
#' fun may be a signed distance function built with [sdf_sphere()] and
#' [sdf_blend()]. levels then defaults to 0, and vertex normals (with the
#' crossing normals of `"dual"`) come from the exact gradient of its
#' expression tree in place of central differences.
#' @inheritParams stat_isosurface_3d
#' @param fun Vectorised scalar function of x, y and z, or a signed distance
#'   function ([sdf_sphere()]).
#' @param args Additional named arguments passed to fun.
#' @param n Grid resolution, scalar or three integers.
#' @param method Extraction method: `"cubes"` (the default on a grid),
#'   `"tetra"`, `"dual"` (the default with `adaptive = TRUE`) or `"nets"`.
#' @param refine Number of regula falsi steps per edge vertex; 0 keeps the
#'   linear interpolation of the gridded values.
#' @param adaptive Whether to extract on an octree refined by interval
#'   arithmetic instead of a grid.
#' @param depth Minimum and maximum octree depth; depth d splits each axis
#'   into 2^d intervals.
#' @param tol Geometric tolerance of the octree, relative to the diagonal of
#'   the scale box.
#' @references Dowell and Jarratt (1971). A modified regula falsi method for
#'   computing the root of an equation. BIT 11, 168-174. \doi{10.1007/BF01934364}
#'
#'   Plantinga and Vegter (2004). Isotopic approximation of implicit curves
#'   and surfaces. Symposium on Geometry Processing, 251-260.
#'   <https://diglib.eg.org/handle/10.2312/SGP.SGP04.251-260>
#'
#'   Ju, Losasso, Schaefer and Warren (2002). Dual contouring of Hermite data.
#'   ACM Transactions on Graphics 21, 339-346. \doi{10.1145/566654.566586}
#' @export
#' @examples
#' ggplot2::ggplot() +
#'   geom_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2,
#'                      n = 10, levels = 0.5) + coord_3d()
#' ggplot2::ggplot() +
#'   stat_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2,
#'                      n = 8, levels = 0.5) + coord_3d()
#' ggplot2::ggplot() +
#'   geom_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2,
#'                      levels = 0.5, adaptive = TRUE, depth = c(2, 4)) +
#'   coord_3d()
stat_isofunction_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                                 position = "identity", ..., fun, n = 24L, args = list(),
                                 levels = NULL, caps = FALSE, method = NULL, refine = 2L,
                                 adaptive = FALSE, depth = c(4L, 8L), tol = 0.002, simplify = NULL, clip = NULL,
                                 na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!is.null(clip)) plane_coefficients_3d(clip, "clip")
  if (!is.logical(adaptive) || length(adaptive) != 1L || is.na(adaptive)) stop("adaptive must be TRUE or FALSE.", call. = FALSE)
  method <- isosurface_method_3d(method %or3d% if (adaptive) "dual" else "cubes")
  if (is.null(levels) && inherits(fun, "ggplot3d_sdf")) levels <- 0
  make_layer_3d(geom, mapping, stats_seed_data_3d(data), StatIsofunction3D, position,
    stats_params_3d(list(fun = fun, n = n, args = args, levels = levels, caps = caps, method = method, refine = refine,
      adaptive = adaptive, depth = depth, tol = tol, simplify = simplify, clip = clip, na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_isofunction_3d
#' @export
geom_isofunction_3d <- function(...) stat_isofunction_3d(...)

check_slices_3d <- function(xslice, yslice, zslice, slice) {
  for (at in list(xslice, yslice, zslice))
    if (!is.null(at) && (!is.numeric(at) || any(!is.finite(at)))) stop("xslice, yslice and zslice must contain finite numbers.", call. = FALSE)
  for (p in slice_plane_list_3d(slice)) plane_coefficients_3d(p, "slice")
}

#' Planar slices through a gridded scalar field
#'
#' Cuts a complete rectilinear x/y/z grid with a numeric `value` aesthetic
#' by planes and colours each section by the field, as MATLAB's `slice`,
#' plotly's isosurface slices and PyVista's `slice()` do. Each plane is
#' sampled on a lattice of points and the field is interpolated trilinearly
#' within its grid cell. A plane perpendicular to an axis (`xslice`,
#' `yslice`, `zslice`, or a `slice` plane with an axis normal) is sampled on
#' the grid's own coordinates along the two other axes, so a slice on a grid
#' layer reproduces the grid values exactly; any other plane is sampled on
#' `n` x `n` points over the rectangle around its section with the grid box,
#' and the lattice triangles are clipped to that section. Triangles with a
#' missing corner value are dropped. With no slice given, one slice per axis
#' passes through the centre of the grid, as in PyVista's
#' `slice_orthogonal()`.
#'
#' Computed variables: `value`, the mean of the three corner values of each
#' triangle (fill is constant per face), and `slice`, a factor naming the
#' plane (`"x = 0.5"`, `"plane 1"`). The default fill is
#' `after_stat(value)`. Slices are not shaded; pass `shade = TRUE` to light
#' them.
#'
#' stat_density_slice_3d() slices the Gaussian product-kernel density of
#' stat_density_3d(), computed on its `n` grid; `density` is the computed
#' density.
#' @inheritParams stat_isosurface_3d
#' @param xslice,yslice,zslice Positions of planes perpendicular to the x, y
#'   and z axes, in transformed position coordinates.
#' @param slice NULL, a plane made with [sdf_plane()], or a list of them, for
#'   slices in any direction.
#' @param n Lattice size per side for oblique planes; NULL uses the largest
#'   grid dimension. For stat_density_slice_3d() the density grid resolution,
#'   which also sets the lattice.
#' @references MathWorks. slice: Volume slice planes.
#'   <https://www.mathworks.com/help/matlab/ref/slice.html>
#'
#'   PyVista. DataObjectFilters.slice_orthogonal.
#'   <https://docs.pyvista.org/api/core/_autosummary/pyvista.dataobjectfilters.slice_orthogonal>
#' @export
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 12),
#'                  y = seq(-1, 1, length.out = 12), z = seq(-1, 1, length.out = 12))
#' d$value <- with(d, exp(-(x^2 + 2 * y^2 + z^2)))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
#'   geom_slice_3d() + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
#'   stat_slice_3d(slice = sdf_plane(c(1, -1, 1)), n = 30) + coord_3d()
#' set.seed(1)
#' p <- data.frame(x = rnorm(200), y = rnorm(200), z = rnorm(200))
#' ggplot2::ggplot(p, ggplot2::aes(x, y, z = z)) +
#'   geom_density_slice_3d(zslice = 0, n = 24) + coord_3d()
stat_slice_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                          position = "identity", ..., xslice = NULL, yslice = NULL, zslice = NULL,
                          slice = NULL, n = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  check_slices_3d(xslice, yslice, zslice, slice)
  make_layer_3d(geom, mapping, data, StatSlice3D, position,
    stats_params_3d(list(xslice = xslice, yslice = yslice, zslice = zslice, slice = slice, n = n,
      na.rm = na.rm, shade = FALSE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_slice_3d
#' @export
geom_slice_3d <- function(...) stat_slice_3d(...)

#' @rdname stat_slice_3d
#' @inheritParams stat_density_3d
#' @export
stat_density_slice_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                                  position = "identity", ..., h = NULL, adjust = 1, n = 32L,
                                  xslice = NULL, yslice = NULL, zslice = NULL, slice = NULL,
                                  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  check_slices_3d(xslice, yslice, zslice, slice)
  make_layer_3d(geom, mapping, data, StatDensitySlice3D, position,
    stats_params_3d(list(h = h, adjust = adjust, n = n, xslice = xslice, yslice = yslice, zslice = zslice, slice = slice,
      na.rm = na.rm, shade = FALSE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_slice_3d
#' @export
geom_density_slice_3d <- function(...) stat_density_slice_3d(...)

#' Contour lines on slices through a gridded scalar field
#'
#' Cuts the field by the planes of [stat_slice_3d()] and traces level lines
#' of the interpolated field on each section by the marching triangles of
#' [geom_contour_3d()]: the field is linear on each section triangle,
#' crossings on shared edges are shared, and segments are chained into open
#' lines and closed loops. Computed variables: `level` (the default colour)
#' and `slice`.
#' @inheritParams stat_slice_3d
#' @inheritParams geom_contour_3d
#' @export
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 12),
#'                  y = seq(-1, 1, length.out = 12), z = seq(-1, 1, length.out = 12))
#' d$value <- with(d, exp(-(x^2 + 2 * y^2 + z^2)))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
#'   geom_slice_3d(zslice = 0) + geom_slice_contour_3d(zslice = 0, bins = 6, colour = "white") +
#'   coord_3d()
stat_slice_contour_3d <- function(mapping = NULL, data = NULL, geom = ggplot2::GeomPath,
                                  position = "identity", ..., xslice = NULL, yslice = NULL, zslice = NULL,
                                  slice = NULL, n = NULL, bins = NULL, binwidth = NULL, breaks = NULL,
                                  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  check_slices_3d(xslice, yslice, zslice, slice)
  make_layer_3d(geom, mapping, data, StatSliceContour3D, position,
    stats_params_3d(list(xslice = xslice, yslice = yslice, zslice = zslice, slice = slice, n = n,
      bins = bins, binwidth = binwidth, breaks = breaks, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_slice_contour_3d
#' @export
geom_slice_contour_3d <- function(...) stat_slice_contour_3d(...)

#' Intersection curves of two implicit surfaces
#'
#' Traces the curve where `fun1 = levels[1]` and `fun2 = levels[2]` meet,
#' inside the box of the position scales. Both functions are evaluated on an
#' `n` grid and each cell is split into the six tetrahedra of
#' `stat_isosurface_3d(method = "tetra")`, on which both are replaced by their
#' linear interpolants. In each tetrahedron the level set of fun1 is a
#' triangle or a quad on which fun2 is linear, so the curve crosses it in one
#' segment; segments end on the faces shared with the neighbouring
#' tetrahedra and are chained into open curves (ending on the box) and
#' closed loops. Each point is then moved onto both level sets by `refine`
#' Newton steps with the minimum-norm update for the two equations, using
#' central differences of the functions, or exact gradients for signed
#' distance functions ([sdf_sphere()]). Points where the two surfaces touch
#' tangentially keep their position.
#'
#' A curve that lies within a single grid cell, or two curves that pass
#' through one cell, can be missed or joined; a finer `n` resolves them.
#' Snyder's interval method subdivides until each box holds one curve piece
#' that is monotone in some coordinate.
#' @inheritParams stat_isofunction_3d
#' @param fun1,fun2 Vectorised scalar functions of x, y and z, or signed
#'   distance functions.
#' @param levels The levels of fun1 and fun2.
#' @param refine Number of Newton steps per curve point; 0 keeps the
#'   intersection of the linear interpolants.
#' @references Snyder (1992). Interval analysis for computer graphics.
#'   SIGGRAPH '92, 121-130. \doi{10.1145/133994.134024}
#' @export
#' @examples
#' ggplot2::ggplot() +
#'   geom_isocurve_3d(fun1 = sdf_sphere(radius = 1),
#'                    fun2 = function(x, y, z) x^2 - y^2 - z * 0.5, n = 16) +
#'   ggplot2::xlim(-1.2, 1.2) + ggplot2::ylim(-1.2, 1.2) +
#'   scale_z_continuous(limits = c(-1.2, 1.2)) + coord_3d()
stat_isocurve_3d <- function(mapping = NULL, data = NULL, geom = ggplot2::GeomPath,
                             position = "identity", ..., fun1, fun2, n = 32L, args = list(),
                             levels = c(0, 0), refine = 2L,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!is.function(fun1) || !is.function(fun2)) stop("fun1 and fun2 must be functions.", call. = FALSE)
  if (!is.numeric(levels) || length(levels) != 2L || any(!is.finite(levels))) stop("levels must be two finite numbers.", call. = FALSE)
  if (!is.numeric(refine) || length(refine) != 1L || !is.finite(refine) || refine < 0 || refine != trunc(refine))
    stop("refine must be a non-negative integer.", call. = FALSE)
  make_layer_3d(geom, mapping, stats_seed_data_3d(data), StatIsocurve3D, position,
    stats_params_3d(list(fun1 = fun1, fun2 = fun2, n = n, args = args, levels = as.double(levels), refine = as.integer(refine),
      na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_isocurve_3d
#' @export
geom_isocurve_3d <- function(...) stat_isocurve_3d(...)

#' Convex hulls in three dimensions
#'
#' Native Quickhull assigns outside points to facets, selects the furthest point
#' of an outside set, replaces visible facets by the horizon cone, and
#' repartitions their outside sets. Duplicate points are removed. Coplanar
#' inputs return a triangulated planar convex polygon; collinear inputs return
#' no faces. Orientation uses a fixed interior point and scale-relative
#' floating-point tolerance. x/y/z positions are already transformed.
#' @inheritParams stat_function_3d
#' @param space `"data"` takes the hull of the positions. `"ilr"` takes the
#'   hull of a composition mapped with `a`, `b`, `c` (and `d`) in isometric
#'   log-ratio coordinates and keeps its facets on the original points.
#' @references Barber, Dobkin and Huhdanpaa (1996). The Quickhull Algorithm for
#'   Convex Hulls. ACM TOMS 22, 469-483. \doi{10.1145/235815.235821}
#' @export
#' @examples
#' d <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_hull_3d(fill = "steelblue") + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   stat_hull_3d(fill = "steelblue") + coord_3d()
stat_hull_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                          position = "identity", ..., space = "data", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatHull3D, position,
    stats_params_3d(list(space = match.arg(space, c("data", "ilr")), na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_hull_3d
#' @export
geom_hull_3d <- function(...) stat_hull_3d(...)
