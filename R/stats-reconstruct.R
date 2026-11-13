utils::globalVariables(c("geometry_delaunay_cpp", "geometry_delaunay3_cpp", "geometry_alpha_shape_cpp", "geometry_rbf_cpp"))

stats_continuous_3d <- function(scales, what) {
  if (any(vapply(scales[c("x", "y", "z")], function(s) !is.null(s) && s$is_discrete(), logical(1))))
    stop(what, " require continuous position scales.", call. = FALSE)
}

# Rows of data as the corners of triangles (one row of indices per triangle).
stats_triangle_rows_3d <- function(data, triangles) {
  rows <- as.vector(t(triangles))
  out <- data[rows, , drop = FALSE]
  out$.face <- rep(seq_len(nrow(triangles)), each = 3L); out$.corner <- rep.int(1:3, nrow(triangles))
  out$.vertex <- rows; out$z_mean <- rep(rowMeans(matrix(data$z[triangles], ncol = 3L)), each = 3L)
  if (!is.null(data$.source)) out$.origin <- data$.source[rows]
  out
}

StatAlphaShape3D <- ggplot2::ggproto("StatAlphaShape3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  compute_group = function(data, scales, radius = NULL) {
    stats_continuous_3d(scales, "Alpha shapes")
    if (!is.null(radius) && (!is.numeric(radius) || length(radius) != 1L || is.na(radius) || radius <= 0))
      stop("radius must be a positive number or NULL.", call. = FALSE)
    faces <- geometry_alpha_shape_cpp(as.matrix(data[c("x", "y", "z")]), if (is.null(radius)) NA_real_ else radius)
    if (!nrow(faces)) return(data.frame())
    out <- stats_triangle_rows_3d(data, faces[, 1:3, drop = FALSE])
    out$radius <- attr(faces, "alpha")
    out$solid <- rep(faces[, 4L] == 1L, each = 3L)
    out
  }
)

# Unit normals of points from the principal axes of their k nearest
# neighbours, oriented consistently by propagation along a minimum spanning
# tree of the neighbour graph weighted by 1 - |n_i . n_j| (Hoppe et al.
# 1992); each tree starts at its highest point with an upward normal.
point_normals_3d <- function(xyz, k) {
  n <- nrow(xyz)
  k <- min(k, n - 1L)
  d2 <- as.matrix(stats::dist(xyz))^2
  near <- t(apply(d2, 1L, function(r) order(r)[seq_len(k + 1L)]))
  normal <- t(vapply(seq_len(n), function(i) {
    p <- xyz[near[i, ], , drop = FALSE]
    e <- eigen(stats::cov(p), symmetric = TRUE)
    e$vectors[, 3L]
  }, numeric(3)))
  weight <- matrix(Inf, n, n)
  edges <- cbind(rep(seq_len(n), each = k), as.vector(t(near[, -1L, drop = FALSE])))
  w <- 1 - abs(rowSums(normal[edges[, 1L], , drop = FALSE] * normal[edges[, 2L], , drop = FALSE]))
  weight[edges] <- w; weight[edges[, 2:1]] <- pmin(weight[edges[, 2:1]], w)
  done <- logical(n); best <- rep(Inf, n); parent <- integer(n)
  for (step in seq_len(n)) {
    open <- which(!done)
    if (all(is.infinite(best[open]))) {
      v <- open[which.max(xyz[open, 3L])]
      if (normal[v, 3L] < 0) normal[v, ] <- -normal[v, ]
    } else {
      v <- open[which.min(best[open])]
      if (sum(normal[v, ] * normal[parent[v], ]) < 0) normal[v, ] <- -normal[v, ]
    }
    done[v] <- TRUE
    closer <- !done & weight[v, ] < best
    best[closer] <- weight[v, closer]; parent[closer] <- v
  }
  normal
}

# Polyharmonic interpolant s(x) = sum_i w_i |x - c_i| + p0 + p . x through
# the points (s = 0) and through off-surface points p_i +- e_i n_i (s = +-
# e_i), the biharmonic fit of Carr et al. (2001). e_i starts at 1 % of the
# bounding box diagonal and is halved until p_i is the data point nearest to
# both of its off-surface points. smooth is added to the diagonal of the
# interpolation matrix.
rbf_fit_3d <- function(xyz, normal, smooth) {
  n <- nrow(xyz)
  diagonal <- sqrt(sum(apply(xyz, 2L, function(a) diff(range(a)))^2))
  eps <- rep(0.01 * diagonal, n)
  nearest <- function(q) max.col(-(outer(rowSums(q^2), rowSums(xyz^2), `+`) - 2 * tcrossprod(q, xyz)), ties.method = "first")
  for (halving in 1:12) {
    bad <- nearest(xyz + normal * eps) != seq_len(n) | nearest(xyz - normal * eps) != seq_len(n)
    if (!any(bad)) break
    eps[bad] <- eps[bad] / 2
  }
  centres <- rbind(xyz, xyz + normal * eps, xyz - normal * eps)
  values <- c(numeric(n), eps, -eps)
  m <- nrow(centres)
  a <- as.matrix(stats::dist(centres)) + diag(smooth, m)
  p <- cbind(1, centres)
  system <- rbind(cbind(a, p), cbind(t(p), matrix(0, 4L, 4L)))
  solution <- tryCatch(solve(system, c(values, numeric(4L))), error = function(e)
    stop("The radial basis function system is singular; remove duplicate points or increase smooth.", call. = FALSE))
  weights <- solution[seq_len(m)]; poly <- solution[m + 1:4]
  function(x, y, z) geometry_rbf_cpp(centres, weights, poly, cbind(x, y, z))
}

StatImplicitFit3D <- ggplot2::ggproto("StatImplicitFit3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  optional_aes = c("nx", "ny", "nz"),
  dropped_aes = c(".source", "value", "weight", "nx", "ny", "nz"),
  compute_group = function(data, scales, method = "rbf", n = 32L, k = 10L, smooth = 0, extractor = "cubes", refine = 2L) {
    stats_continuous_3d(scales, "Implicit surface fits")
    method <- match.arg(method, "rbf")
    if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k < 3 || k != trunc(k)) stop("k must be an integer of at least three.", call. = FALSE)
    if (!is.numeric(smooth) || length(smooth) != 1L || !is.finite(smooth) || smooth < 0) stop("smooth must be a non-negative number.", call. = FALSE)
    xyz <- as.matrix(data[c("x", "y", "z")])
    keep <- stats::complete.cases(xyz) & !duplicated(xyz)
    xyz <- xyz[keep, , drop = FALSE]
    if (nrow(xyz) < 4L) stop("An implicit surface fit needs at least four distinct points.", call. = FALSE)
    if (nrow(xyz) > 2000L) stop("The dense radial basis function solve takes at most 2000 points; thin the data first.", call. = FALSE)
    normal <- if (all(c("nx", "ny", "nz") %in% names(data))) {
      given <- as.matrix(data[keep, c("nx", "ny", "nz")])
      given / sqrt(rowSums(given^2))
    } else point_normals_3d(xyz, as.integer(k))
    if (any(!is.finite(normal))) stop("Point normals must be finite and nonzero.", call. = FALSE)
    if (!(length(n) %in% c(1L, 3L)) || any(!is.finite(n) | n < 2 | n != trunc(n))) stop("n must contain integers of at least two, one per grid axis.", call. = FALSE)
    fun <- rbf_fit_3d(xyz, normal, smooth)
    span <- apply(xyz, 2L, function(a) diff(range(a)))
    pad <- 0.1 * span + 0.05 * sqrt(sum(span^2))
    axes <- lapply(1:3, function(a) seq(min(xyz[, a]) - pad[a], max(xyz[, a]) + pad[a], length.out = rep_len(n, 3L)[a]))
    grid <- stats::setNames(expand.grid(axes), c("x", "y", "z"))
    grid$value <- fun(grid$x, grid$y, grid$z)
    stats_field_3d(grid, 0, FALSE, extractor, fun, list(), as.integer(refine))
  }
)
