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
#' before extraction. Each cell is split into six tetrahedra sharing a fixed
#' body diagonal, giving consistent face diagonals across adjacent cells.
#' Linear interpolation within tetrahedra avoids ambiguous cube cases.
#' Field values within 64 machine epsilons of a threshold, relative to the
#' maximum absolute field value or threshold, are snapped to that threshold.
#' This gives shared grid vertices at numerically coincident intersections.
#' Finite-difference field gradients are interpolated and normalised at vertices
#' (nx, ny, nz). The public face emitter currently uses geometric face normals.
#' caps = TRUE closes the sublevel region on the grid box with outward faces.
#' Surface triangle winding points towards increasing field value.
#'
#' Default levels are interior pretty(range(value), n = 10) values. The number
#' of levels depends on the pretty spacing. level and z_mean are computed
#' face attributes; .face, .corner and .vertex preserve triangle topology.
#' @inheritParams stat_function_3d
#' @param levels Numeric scalar-field thresholds; NULL uses pretty levels.
#' @param caps Whether to cap the sublevel region on the box faces.
#' @references Doi and Koide (1991), An Efficient Method of Triangulating
#'   Equi-Valued Surfaces by Using Tetrahedral Cells. IEICE E74-D, 214-224.
#'   <https://globals.ieice.org/en_transactions/information/10.1587/e74-d_1_214/_p>
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
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatIsosurface3D, position,
    stats_params_3d(list(levels = levels, caps = caps, na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
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
                             levels = NULL, caps = FALSE, na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatDensity3D, position,
    stats_params_3d(list(h = h, adjust = adjust, n = n, levels = levels, caps = caps,
      na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_density_3d
#' @export
geom_isodensity_3d <- function(...) stat_density_3d(...)

#' Level surfaces of a trivariate function
#'
#' Evaluates fun on transformed x/y/z scale ranges and passes the scalar field
#' to the same tetrahedral extraction used by stat_isosurface_3d.
#' @inheritParams stat_isosurface_3d
#' @param fun Vectorised scalar function of x, y and z.
#' @param args Additional named arguments passed to fun.
#' @param n Grid resolution, scalar or three integers.
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
                                 levels = NULL, caps = FALSE, na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, stats_seed_data_3d(data), StatIsofunction3D, position,
    stats_params_3d(list(fun = fun, n = n, args = args, levels = levels, caps = caps,
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
#' @references Barber, Dobkin and Huhdanpaa (1996). The Quickhull Algorithm for
#'   Convex Hulls. ACM TOMS 22, 469-483. <https://doi.org/10.1145/235815.235821>
#' @export
#' @examples
#' d <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_hull_3d(fill = "steelblue") + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   stat_hull_3d(fill = "steelblue") + coord_3d()
stat_hull_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                          position = "identity", ..., na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatHull3D, position,
    stats_params_3d(list(na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_hull_3d
#' @export
geom_hull_3d <- function(...) stat_hull_3d(...)
