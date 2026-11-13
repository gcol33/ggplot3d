#' Alpha shapes of point clouds
#'
#' Computes the Delaunay tetrahedralisation of the points and draws the
#' boundary of their alpha complex (Edelsbrunner and Mucke 1994). A
#' tetrahedron belongs to the complex when its circumradius is below
#' `radius`; a triangle belongs to it when it is a face of such a
#' tetrahedron, or when its own circumradius is below `radius` and its
#' smallest circumsphere contains no other point. Triangles of the complex
#' that are not shared by two of its tetrahedra are drawn, so the shape is the
#' surface of the union of small tetrahedra plus any thin sheets.
#' `radius = Inf` gives the convex hull; a small radius leaves only isolated
#' pieces.
#'
#' The tetrahedralisation is a native Bowyer-Watson insertion with exact
#' orientation and in-sphere predicates, so it is the Delaunay
#' tetrahedralisation of the exact positions (ties among cospherical points
#' are broken by insertion order). Positions are taken after scale
#' transformation, so `radius` is in transformed position units. Triangles
#' bounding a tetrahedron face away from it (computed `solid` is `TRUE`);
#' sheets have no inside. The computed `radius` holds the value used.
#' @inheritParams stat_hull_3d
#' @param radius The alpha of the alpha shape, as a radius in position
#'   units. `NULL` uses the smallest radius at which the tetrahedra of the
#'   complex touch every point. (The argument is not called `alpha` because
#'   that name sets opacity.)
#' @references Edelsbrunner and Mucke (1994). Three-dimensional alpha shapes.
#'   ACM Transactions on Graphics 13, 43-72. \doi{10.1145/174462.156635}
#'
#'   Shewchuk (1997). Adaptive precision floating-point arithmetic and fast
#'   robust geometric predicates. Discrete & Computational Geometry 18,
#'   305-363. \doi{10.1007/PL00009321}
#' @export
#' @examples
#' set.seed(1)
#' u <- runif(300, 0, 2 * pi); v <- runif(300, 0, 2 * pi)
#' d <- data.frame(x = (1 + 0.4 * cos(v)) * cos(u), y = (1 + 0.4 * cos(v)) * sin(u),
#'                 z = 0.4 * sin(v))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_alpha_shape_3d(radius = 0.35, fill = "steelblue") + coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   stat_alpha_shape_3d(fill = "steelblue") + coord_3d()
stat_alpha_shape_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                                position = "identity", ..., radius = NULL, na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatAlphaShape3D, position,
    stats_params_3d(list(radius = radius, na.rm = na.rm, shade = TRUE), list(...)), show.legend, inherit.aes)
}

#' @rdname stat_alpha_shape_3d
#' @export
geom_alpha_shape_3d <- function(...) stat_alpha_shape_3d(...)

#' Implicit surfaces fitted to point clouds
#'
#' Fits a function s(x) whose zero set passes through the points and draws
#' that level surface. `method = "rbf"` is the polyharmonic radial basis
#' function fit of Carr et al. (2001): s(x) = sum_i w_i |x - c_i| + p(x)
#' with a linear polynomial p, the biharmonic spline in three dimensions,
#' interpolating s = 0 at every point and s = +e and -e at two off-surface
#' points moved by e along the point's normal outwards and inwards. e starts
#' at 1 % of the bounding box diagonal and is halved for a point until that
#' point is the data point nearest to both of its off-surface points. The
#' (3n + 4)-square system is solved densely, so at most 2000 points are taken.
#'
#' Normals come from the `nx`, `ny` and `nz` aesthetics when all three are
#' mapped. Otherwise each point's normal is the least principal axis of its
#' `k` nearest neighbours, and normals are oriented consistently by
#' propagation along a minimum spanning tree of the neighbour graph weighted
#' by 1 - |n_i . n_j|, starting at the highest point with an upward normal
#' (Hoppe et al. 1992); a closed surface then gets outward normals.
#'
#' s is sampled on an `n`-point grid over the bounding box of the points,
#' enlarged on each side by 10 % of its extent plus 5 % of its diagonal, and
#' its zero set is extracted as in stat_isofunction_3d with `extractor` as
#' the extraction method, `refine` regula falsi steps per edge vertex and
#' vertex normals from s. Surface triangles face increasing s, outwards.
#' @inheritParams stat_isosurface_3d
#' @param method Fitting method; `"rbf"`.
#' @param n Grid resolution for the extraction, scalar or three integers.
#' @param k Number of neighbours for normal estimation.
#' @param smooth Value added to the diagonal of the interpolation matrix;
#'   positive values trade interpolation for smoothness.
#' @param extractor Isosurface extraction method, as `method` in
#'   [stat_isosurface_3d()].
#' @param refine Number of regula falsi steps per edge vertex.
#' @references Carr, Beatson, Cherrie, Mitchell, Fright, McCallum and Evans
#'   (2001). Reconstruction and representation of 3D objects with radial
#'   basis functions. SIGGRAPH 2001, 67-76. \doi{10.1145/383259.383266}
#'
#'   Hoppe, DeRose, Duchamp, McDonald and Stuetzle (1992). Surface
#'   reconstruction from unorganized points. SIGGRAPH 1992, 71-78.
#'   \doi{10.1145/133994.134011}
#' @export
#' @examples
#' set.seed(2)
#' p <- matrix(rnorm(450), ncol = 3)
#' p <- p / sqrt(rowSums(p^2))
#' d <- data.frame(x = p[, 1], y = p[, 2], z = 0.6 * p[, 3])
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   stat_implicit_fit_3d(n = 20, fill = "steelblue") + coord_3d()
stat_implicit_fit_3d <- function(mapping = NULL, data = NULL, geom = GeomStatsSurface3D,
                                 position = "identity", ..., method = "rbf", n = 32L, k = 10L,
                                 smooth = 0, extractor = "cubes", refine = 2L, na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatImplicitFit3D, position,
    stats_params_3d(list(method = match.arg(method, "rbf"), n = n, k = k, smooth = smooth,
      extractor = isosurface_method_3d(extractor), refine = refine, na.rm = na.rm, shade = TRUE), list(...)),
    show.legend, inherit.aes)
}
