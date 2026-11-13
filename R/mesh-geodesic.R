utils::globalVariables("mesh_geodesic_path_cpp")

# Points given as c(x, y, z), or a matrix or data frame with one point per
# row (columns x, y, z when named).
mesh_points_3d <- function(x, name) {
  if (is.data.frame(x)) x <- as.matrix(x[if (all(c("x", "y", "z") %in% names(x))) c("x", "y", "z") else seq_len(min(3L, ncol(x)))])
  if (is.numeric(x) && is.null(dim(x)) && length(x) == 3L) x <- matrix(x, 1L)
  if (!is.numeric(x) || !is.matrix(x) || ncol(x) != 3L || !nrow(x) || any(!is.finite(x)))
    stop(name, " must be a point c(x, y, z) or a matrix or data frame of points.", call. = FALSE)
  matrix(as.double(x), ncol = 3L)
}

# The .mesh part whose closest vertex is nearest to each point.
mesh_point_owner_3d <- function(m, points) {
  part <- m$.mesh %or3d% rep(1L, nrow(m))
  apply(points, 1L, function(p) {
    d <- (m$x - p[1L])^2 + (m$y - p[2L])^2 + (m$z - p[3L])^2
    part[which.min(d)]
  })
}

#' Intrinsic geometry on meshes
#'
#' Distances, shortest paths and polar charts measured along a triangulated
#' surface rather than through space. The verbs take a [mesh_3d()] corner
#' table or a compatible data frame, as the [mesh processing][mesh_processing]
#' verbs do, and points in the units of its positions.
#'
#' `mesh_geodesic_distance()` adds the column `distance`, the geodesic
#' distance from the nearest source, by the heat method of Crane, Weischedel
#' and Wardetzky (2013). Heat spreads from the sources for a short time
#' `t = time * h^2`, `h` the mean edge length, by one backward Euler step of
#' the heat equation with the cotangent Laplacian and mixed Voronoi areas;
#' the direction of the heat gradient, normalised on every face, is then
#' integrated back into a distance by a Poisson equation. Both linear systems
#' are factored by a native sparse Cholesky decomposition with a nested
#' dissection ordering. A source is a point; its unit of heat starts on the
#' corners of the closest face, by barycentric weight, and the distance is
#' shifted to vanish there. With several sources the distance is to the
#' nearest. On a surface with boundary, `boundary` chooses how heat behaves
#' there: absorbed (`"dirichlet"`), reflected (`"neumann"`), or the mean of
#' both (`"average"`), which the paper recommends because it mimics a surface
#' without boundary. Parts of the mesh that hold no source get `NA`.
#'
#' `mesh_logmap()` adds `logmap_u` and `logmap_v`, the logarithmic map of the
#' surface at the vertex closest to `source`: polar coordinates around the
#' source whose radius is the geodesic distance and whose angle is the
#' direction in which the shortest geodesic leaves the source, measured from
#' `direction` (projected into the tangent plane; by default the first edge
#' at the source). It is the vector heat method of Sharp, Soliman and Crane
#' (2019): the source's tangent vectors are carried over the surface by heat
#' flow of the connection Laplacian, a unit vector to fix the angle zero and
#' outward unit vectors on a small circle to give the radial direction, and
#' the radius comes from a Poisson equation on the radial field.
#'
#' `mesh_geodesic_path()` returns the shortest path along the surface between
#' the vertices closest to each row of `from` and `to`, as a polyline that
#' crosses the faces. A Dijkstra path along the mesh edges is straightened by
#' FlipOut (Sharp and Crane 2020): edges of an intrinsic triangulation, which
#' keeps the surface fixed and changes only which straight segments on it are
#' edges, are flipped around every bend of the path that is sharper than a
#' straight line, until no bend can be shortened. The result is a locally
#' shortest geodesic of the polyhedral surface, exact up to rounding; it is
#' the shortest path whenever the Dijkstra path winds around the surface's
#' holes the same way the shortest path does. The intrinsic edges are traced
#' back onto the input faces through the polar angles of the edge directions
#' at each vertex (signposts, Sharp, Soliman and Crane 2019b).
#' @param m A mesh: a [mesh_3d()] corner table or a compatible data frame.
#' @param source Source points, `c(x, y, z)` or a matrix or data frame of
#'   points; `mesh_logmap()` takes one point.
#' @param time Heat time as a multiple of the squared mean edge length.
#'   Larger values give a smoother distance.
#' @param boundary Heat at boundary vertices: `"average"`, `"neumann"` or
#'   `"dirichlet"`.
#' @param direction Tangent direction of the logarithmic map's angle zero,
#'   `c(x, y, z)`, or NULL for the first edge at the source.
#' @param from,to Path ends, as points; rows pair up.
#' @param max_iterations Largest number of FlipOut steps per path.
#' @return `mesh_geodesic_distance()` and `mesh_logmap()` return the mesh of
#'   the class of `m` with the added columns. `mesh_geodesic_path()` returns a
#'   data frame of path points with columns `x`, `y`, `z`, `path` (the row of
#'   `from` and `to`) and `s`, the arc length from the start; the attribute
#'   `length` holds the length of each path.
#' @references
#'   Crane, Weischedel and Wardetzky (2013). Geodesics in heat: a new approach
#'   to computing distance based on heat flow. ACM Transactions on Graphics
#'   32(5), 152. \doi{10.1145/2516971.2516977}
#'
#'   Sharp, Soliman and Crane (2019). The vector heat method. ACM
#'   Transactions on Graphics 38(3), 24. \doi{10.1145/3243651}
#'
#'   Sharp, Soliman and Crane (2019b). Navigating intrinsic triangulations.
#'   ACM Transactions on Graphics 38(4), 55. \doi{10.1145/3306346.3322979}
#'
#'   Sharp and Crane (2020). You can find geodesic paths in triangle meshes by
#'   just flipping edges. ACM Transactions on Graphics 39(6), 249.
#'   \doi{10.1145/3414685.3417839}
#'
#'   Davis (2006). Direct Methods for Sparse Linear Systems. SIAM.
#'   \doi{10.1137/1.9780898718881}
#' @examples
#' sphere <- function(u, v) list(x = cos(u) * sin(v), y = sin(u) * sin(v), z = cos(v))
#' m <- ggplot2::layer_data(ggplot2::ggplot() +
#'   stat_parametric_3d(fun = sphere, v = c(0, pi), n = c(32, 17), closed = c(TRUE, FALSE)) +
#'   coord_3d())
#' m <- m[c("x", "y", "z", ".face", ".corner", ".vertex")]
#' d <- mesh_geodesic_distance(m, source = c(0, 0, 1))
#' range(d$distance)
#' l <- mesh_logmap(m, source = c(0, 0, 1), direction = c(1, 0, 0))
#' head(l[c("logmap_u", "logmap_v")])
#' path <- mesh_geodesic_path(m, from = c(1, 0, 0), to = c(0, 1, 0))
#' attr(path, "length")
#' @name mesh_geodesic
NULL

#' @rdname mesh_geodesic
#' @export
mesh_geodesic_distance <- function(m, source, time = 1, boundary = c("average", "neumann", "dirichlet")) {
  source <- mesh_points_3d(source, "source")
  time <- mesh_number_3d(time, "time", function(x) is.finite(x) && x > 0, "positive")
  boundary <- match.arg(boundary)
  m <- mesh_input_3d(m)
  owner <- mesh_point_owner_3d(m, source)
  mesh_run_3d(m, "geodesic_distance", function(part) list(source = source[owner == part$.mesh[1L], , drop = FALSE],
    time = time, boundary = boundary), rebuild = FALSE)
}

#' @rdname mesh_geodesic
#' @export
mesh_logmap <- function(m, source, direction = NULL, time = 1) {
  source <- mesh_points_3d(source, "source")
  if (nrow(source) != 1L) stop("source must be a single point.", call. = FALSE)
  if (!is.null(direction) && (!is.numeric(direction) || length(direction) != 3L || any(!is.finite(direction)) || all(direction == 0)))
    stop("direction must be a nonzero vector c(x, y, z).", call. = FALSE)
  time <- mesh_number_3d(time, "time", function(x) is.finite(x) && x > 0, "positive")
  m <- mesh_input_3d(m)
  owner <- mesh_point_owner_3d(m, source)
  mesh_run_3d(m, "logmap", function(part) list(source = if (owner == part$.mesh[1L]) source[1L, ] else numeric(),
    direction = as.double(direction %or3d% numeric()), time = time), rebuild = FALSE)
}

#' @rdname mesh_geodesic
#' @export
mesh_geodesic_path <- function(m, from, to, max_iterations = 100000L) {
  from <- mesh_points_3d(from, "from"); to <- mesh_points_3d(to, "to")
  if (nrow(from) != nrow(to)) {
    if (nrow(from) == 1L) from <- from[rep(1L, nrow(to)), , drop = FALSE]
    else if (nrow(to) == 1L) to <- to[rep(1L, nrow(from)), , drop = FALSE]
    else stop("from and to must have the same number of points, or one of them a single point.", call. = FALSE)
  }
  max_iterations <- mesh_count_3d(max_iterations, "max_iterations", 1L)
  m <- as.data.frame(mesh_input_3d(m))
  if (is.null(m$.mesh)) m$.mesh <- 1L
  m <- m[order(m$.mesh, m$.face, m$.corner), , drop = FALSE]
  owner <- mesh_point_owner_3d(m, from)
  pieces <- lapply(seq_len(nrow(from)), function(i) {
    a <- mesh_arrays_3d(m[m$.mesh == owner[i], , drop = FALSE])
    res <- mesh_geodesic_path_cpp(a$xyz, a$triangles, from[i, , drop = FALSE], to[i, , drop = FALSE], max_iterations)
    if (is.na(res$length)) warning("No path joins point ", i, " of from and to on the surface.", call. = FALSE)
    s <- cumsum(c(0, sqrt(diff(res$x)^2 + diff(res$y)^2 + diff(res$z)^2)))[seq_along(res$x)]
    list(points = data.frame(x = res$x, y = res$y, z = res$z, path = rep(i, length(res$x)), s = s), length = res$length)
  })
  out <- do.call(rbind, lapply(pieces, `[[`, "points"))
  rownames(out) <- NULL
  attr(out, "length") <- vapply(pieces, `[[`, numeric(1), "length")
  out
}
