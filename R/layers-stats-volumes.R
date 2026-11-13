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
#' @param caps Whether to cap the sublevel region on the box faces.
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
                                method = "cubes", simplify = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatIsosurface3D, position,
    stats_params_3d(list(levels = levels, caps = caps, method = isosurface_method_3d(method), simplify = simplify, na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
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
                             levels = NULL, caps = FALSE, space = "data", method = "cubes", simplify = NULL,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatDensity3D, position,
    stats_params_3d(list(h = h, adjust = adjust, n = n, levels = levels, caps = caps, method = isosurface_method_3d(method), simplify = simplify,
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
#' @inheritParams stat_isosurface_3d
#' @param fun Vectorised scalar function of x, y and z.
#' @param args Additional named arguments passed to fun.
#' @param n Grid resolution, scalar or three integers.
#' @param refine Number of regula falsi steps per edge vertex; 0 keeps the
#'   linear interpolation of the gridded values.
#' @references Dowell and Jarratt (1971). A modified regula falsi method for
#'   computing the root of an equation. BIT 11, 168-174. \doi{10.1007/BF01934364}
#' @export
#' @examples
#' ggplot2::ggplot() +
#'   geom_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2,
#'                      n = 10, levels = 0.5) + coord_3d()
#' ggplot2::ggplot() +
#'   stat_isofunction_3d(fun = function(x, y, z) x^2 + y^2 + z^2,
#'                      n = 8, levels = 0.5) + coord_3d()
stat_isofunction_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                                 position = "identity", ..., fun, n = 24L, args = list(),
                                 levels = NULL, caps = FALSE, method = "cubes", refine = 2L, simplify = NULL,
                                 na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, stats_seed_data_3d(data), StatIsofunction3D, position,
    stats_params_3d(list(fun = fun, n = n, args = args, levels = levels, caps = caps, method = isosurface_method_3d(method), refine = refine, simplify = simplify,
      na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_isofunction_3d
#' @export
geom_isofunction_3d <- function(...) stat_isofunction_3d(...)

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
