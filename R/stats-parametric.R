utils::globalVariables(c("parametric_grid_cpp", "parametric_edges_cpp", "parametric_bisect_cpp", "parametric_face_normals_cpp"))

parametric_pair_3d <- function(x, name, test) {
  if (length(x) == 1L) x <- rep(x, 2L)
  if (length(x) != 2L || !test(x)) stop(name, " must give one value for u and one for v.", call. = FALSE)
  if (!is.null(names(x)) && all(names(x) %in% c("u", "v"))) x <- x[c("u", "v")]
  unname(x)
}

parametric_evaluator_3d <- function(fun, args) {
  function(uv) {
    r <- do.call(fun, c(list(uv[, 1L], uv[, 2L]), args))
    if (is.matrix(r) && ncol(r) == 3L) r <- list(r[, 1L], r[, 2L], r[, 3L])
    if (!is.list(r) || length(r) != 3L) stop("fun must return x, y and z as a list, data frame or three-column matrix.", call. = FALSE)
    r <- if (all(c("x", "y", "z") %in% names(r))) r[c("x", "y", "z")] else r
    out <- vapply(r, function(a) rep_len(as.double(a), nrow(uv)), numeric(nrow(uv)))
    matrix(out, nrow = nrow(uv))
  }
}

# Position and first and second partial derivatives of the surface at the
# rows of uv, by central differences with steps h (one per parameter, or a
# two-column matrix with one row per point) in one call to fun over a
# nine-point stencil; second = FALSE stops at the five points of the first
# derivatives.
parametric_derivatives_3d <- function(evaluate, uv, h, second = TRUE) {
  k <- nrow(uv)
  h <- if (is.matrix(h)) h else matrix(h, k, 2L, byrow = TRUE)
  offsets <- rbind(c(0, 0), c(1, 0), c(-1, 0), c(0, 1), c(0, -1), c(1, 1), c(1, -1), c(-1, 1), c(-1, -1))
  stencil <- do.call(rbind, lapply(seq_len(if (second) 9L else 5L), function(s) uv + h * rep(offsets[s, ], each = k)))
  r <- evaluate(stencil)
  at <- function(s) r[(s - 1L) * k + seq_len(k), , drop = FALSE]
  out <- list(r = at(1), ru = (at(2) - at(3)) / (2 * h[, 1L]), rv = (at(4) - at(5)) / (2 * h[, 2L]))
  if (second) {
    out$ruu <- (at(2) - 2 * at(1) + at(3)) / h[, 1L]^2
    out$rvv <- (at(4) - 2 * at(1) + at(5)) / h[, 2L]^2
    out$ruv <- (at(6) - at(7) - at(8) + at(9)) / (4 * h[, 1L] * h[, 2L])
  }
  out
}

# Unit normal r_u x r_v, Gaussian and mean curvature from the first and second
# fundamental forms.
parametric_frame_3d <- function(evaluate, uv, h) {
  k <- nrow(uv)
  if (!k) return(list(normal = matrix(0, 0, 3), gaussian = numeric(), mean = numeric()))
  d <- parametric_derivatives_3d(evaluate, uv, h)
  ru <- d$ru; rv <- d$rv; ruu <- d$ruu; rvv <- d$rvv; ruv <- d$ruv
  n <- cbind(ru[, 2] * rv[, 3] - ru[, 3] * rv[, 2], ru[, 3] * rv[, 1] - ru[, 1] * rv[, 3], ru[, 1] * rv[, 2] - ru[, 2] * rv[, 1])
  size <- sqrt(rowSums(n^2))
  n <- n / ifelse(size > 0, size, NA_real_)
  E <- rowSums(ru * ru); F <- rowSums(ru * rv); G <- rowSums(rv * rv)
  L <- rowSums(ruu * n); M <- rowSums(ruv * n); N <- rowSums(rvv * n)
  det <- E * G - F^2
  list(normal = n, gaussian = (L * N - M^2) / det, mean = (E * N - 2 * F * M + G * L) / (2 * det))
}

parametric_surface_3d <- function(fun, u, v, n, closed, twist, args, adaptive, tolerance, angle, max_depth) {
  if (!is.function(fun)) stop("fun must be a function of u and v.", call. = FALSE)
  range_ok <- function(r) is.numeric(r) && length(r) == 2L && all(is.finite(r)) && r[2L] > r[1L]
  if (!range_ok(u) || !range_ok(v)) stop("u and v must be increasing finite ranges.", call. = FALSE)
  n <- parametric_pair_3d(n, "n", function(x) is.numeric(x) && all(is.finite(x) & x >= 2 & x == trunc(x)))
  closed <- parametric_pair_3d(closed, "closed", function(x) is.logical(x) && !anyNA(x))
  twist <- parametric_pair_3d(twist, "twist", function(x) is.logical(x) && !anyNA(x))
  if (any(twist & !closed)) stop("A twisted direction must also be closed.", call. = FALSE)
  if (any(closed & n < 3)) stop("A closed direction needs at least three samples.", call. = FALSE)
  evaluate <- parametric_evaluator_3d(fun, args)
  step <- c(diff(u) / (n[1L] - !closed[1L]), diff(v) / (n[2L] - !closed[2L]))
  samples <- as.matrix(expand.grid(u[1L] + step[1L] * (seq_len(n[1L]) - 1L), v[1L] + step[2L] * (seq_len(n[2L]) - 1L)))
  xyz <- evaluate(samples)
  if (any(!is.finite(xyz))) stop("fun must return finite coordinates on the parameter grid.", call. = FALSE)
  grid <- parametric_grid_cpp(as.integer(n), closed, twist, xyz, c(u[1L], step[1L]), c(v[1L], step[2L]))
  xyz <- xyz[grid$sample, , drop = FALSE]
  uv <- samples[grid$sample, , drop = FALSE]
  free <- grid$free; tri <- grid$triangles; cu <- grid$u; cv <- grid$v
  h <- c(diff(u), diff(v)) * 1e-4
  frame <- parametric_frame_3d(evaluate, uv, h)
  if (isTRUE(adaptive)) {
    diagonal <- sqrt(sum(apply(xyz, 2L, function(a) diff(range(a)))^2))
    for (depth in seq_len(max_depth)) {
      edges <- parametric_edges_cpp(tri, cu, cv, free)
      a <- edges$ends[, 1L]; b <- edges$ends[, 2L]
      mid <- evaluate(edges$uv)
      deviation <- sqrt(rowSums((mid - (xyz[a, , drop = FALSE] + xyz[b, , drop = FALSE]) / 2)^2))
      turn <- acos(pmin(1, pmax(-1, rowSums(frame$normal[a, , drop = FALSE] * frame$normal[b, , drop = FALSE])))) * 180 / pi
      marked <- (is.finite(deviation) & deviation > tolerance * diagonal) | (is.finite(turn) & turn > angle)
      if (!any(marked)) break
      split <- parametric_bisect_cpp(tri, cu, cv, free, xyz, marked)
      added <- split$split
      xyz <- rbind(xyz, mid[added, , drop = FALSE])
      uv <- rbind(uv, edges$uv[added, , drop = FALSE])
      free <- rbind(free, matrix(FALSE, length(added), 2L))
      more <- parametric_frame_3d(evaluate, edges$uv[added, , drop = FALSE], h)
      frame <- list(normal = rbind(frame$normal, more$normal), gaussian = c(frame$gaussian, more$gaussian),
        mean = c(frame$mean, more$mean))
      tri <- split$triangles; cu <- split$u; cv <- split$v
    }
  }
  normal <- frame$normal
  missing <- !is.finite(rowSums(normal))
  if (any(missing)) normal[missing, ] <- parametric_face_normals_cpp(tri, xyz)[missing, , drop = FALSE]
  rows <- as.vector(t(tri))
  face_mean <- function(x) {
    m <- matrix(x, ncol = 3L, byrow = TRUE)
    rep(rowMeans(ifelse(is.finite(m), m, NA_real_), na.rm = TRUE), each = 3L)
  }
  out <- data.frame(x = xyz[rows, 1L], y = xyz[rows, 2L], z = xyz[rows, 3L],
    nx = normal[rows, 1L], ny = normal[rows, 2L], nz = normal[rows, 3L],
    u = face_mean(as.vector(t(cu))), v = face_mean(as.vector(t(cv))),
    gaussian_curvature = face_mean(frame$gaussian[rows]), mean_curvature = face_mean(frame$mean[rows]),
    .face = rep(seq_len(nrow(tri)), each = 3L), .corner = rep.int(1:3, nrow(tri)), .vertex = rows)
  out$z_mean <- face_mean(out$z)
  out
}

StatParametric3D <- ggplot2::ggproto("StatParametric3D", StatGeometry3D,
  required_aes = character(),
  optional_aes = c("x", "y", "z"),
  compute_group = function(data, scales, fun, u = c(0, 2 * pi), v = c(0, 2 * pi), n = 40L,
                           closed = FALSE, twist = FALSE, args = list(), adaptive = FALSE,
                           tolerance = 0.002, angle = 10, max_depth = 4L) {
    parametric_surface_3d(fun, u, v, n, closed, twist, args, adaptive, tolerance, angle, max_depth)
  }
)

# Geodesics of a parametric surface r(u, v) as the flow of the Hamiltonian
# H(q, p) = 1/2 p' g(q)^-1 p, g the first fundamental form, whose
# trajectories are geodesics run at the speed sqrt(2 H) = 1. Each step is
# the generalised Stoermer-Verlet method (Hairer, Lubich and Wanner 2006,
# section VI.3), symplectic and symmetric, with its implicit stages solved
# by fixed-point iteration; three steps of sizes c1 h, c0 h, c1 h (Yoshida
# 1990) compose it to order four. Metric derivatives come from central
# differences of fun with steps 1e-4 times max(1, |q|).
parametric_geodesics_3d <- function(fun, start, angle, length, n, args) {
  evaluate <- parametric_evaluator_3d(fun, args)
  start <- if (is.null(dim(start))) matrix(start, 1L) else as.matrix(start)
  if (!is.numeric(start) || ncol(start) != 2L || !nrow(start) || any(!is.finite(start))) stop("start must be c(u, v) or a two-column matrix of parameters.", call. = FALSE)
  if (!is.numeric(angle) || !length(angle) || any(!is.finite(angle))) stop("angle must be finite numbers.", call. = FALSE)
  length <- mesh_number_3d(length, "length", function(x) is.finite(x) && x > 0, "positive")
  n <- mesh_count_3d(n, "n", 1L)
  rays <- expand.grid(start = seq_len(nrow(start)), angle = seq_along(angle))
  q <- start[rays$start, , drop = FALSE]; k <- nrow(q)
  metric <- function(q, second) {
    d <- parametric_derivatives_3d(evaluate, q, 1e-4 * pmax(abs(q), 1), second)
    g <- list(E = rowSums(d$ru^2), F = rowSums(d$ru * d$rv), G = rowSums(d$rv^2), d = d)
    if (second) {
      g$Eu <- 2 * rowSums(d$ru * d$ruu); g$Ev <- 2 * rowSums(d$ru * d$ruv)
      g$Fu <- rowSums(d$ruu * d$rv + d$ru * d$ruv); g$Fv <- rowSums(d$ruv * d$rv + d$ru * d$rvv)
      g$Gu <- 2 * rowSums(d$rv * d$ruv); g$Gv <- 2 * rowSums(d$rv * d$rvv)
    }
    if (any(!is.finite(g$E * g$G - g$F^2)) || any(g$E * g$G - g$F^2 <= 0)) stop("The geodesic reached a point where the parametrisation is singular.", call. = FALSE)
    g
  }
  velocity <- function(p, g) { det <- g$E * g$G - g$F^2; cbind(g$G * p[, 1L] - g$F * p[, 2L], g$E * p[, 2L] - g$F * p[, 1L]) / det }
  force <- function(p, g) {
    w <- velocity(p, g)
    0.5 * cbind(w[, 1L]^2 * g$Eu + 2 * w[, 1L] * w[, 2L] * g$Fu + w[, 2L]^2 * g$Gu,
                 w[, 1L]^2 * g$Ev + 2 * w[, 1L] * w[, 2L] * g$Fv + w[, 2L]^2 * g$Gv)
  }
  settle <- function(x, update) {
    for (i in seq_len(50L)) {
      nx <- update(x)
      if (max(abs(nx - x) / (1 + abs(nx))) < 1e-13) return(nx)
      x <- nx
    }
    x
  }
  verlet <- function(q, p, h) {
    g0 <- metric(q, TRUE)
    half <- settle(p, function(x) p + h / 2 * force(x, g0))
    v0 <- velocity(half, g0)
    q1 <- settle(q + h * v0, function(x) q + h / 2 * (v0 + velocity(half, metric(x, FALSE))))
    list(q = q1, p = half + h / 2 * force(half, metric(q1, TRUE)))
  }
  g <- metric(q, FALSE)
  e1 <- g$d$ru / sqrt(g$E)
  e2 <- g$d$rv - e1 * rowSums(g$d$rv * e1); e2 <- e2 / sqrt(rowSums(e2^2))
  a <- angle[rays$angle]
  tangent <- e1 * cos(a) + e2 * sin(a)
  p <- cbind(rowSums(g$d$ru * tangent), rowSums(g$d$rv * tangent))
  h <- length / n
  c1 <- 1 / (2 - 2^(1 / 3)); c0 <- 1 - 2 * c1
  path <- array(NA_real_, c(n + 1L, k, 2L)); path[1L, , ] <- q
  for (step in seq_len(n)) {
    for (c in c(c1, c0, c1)) { s <- verlet(q, p, c * h); q <- s$q; p <- s$p }
    path[step + 1L, , ] <- q
  }
  uv <- cbind(as.vector(path[, , 1L]), as.vector(path[, , 2L]))
  xyz <- evaluate(uv)
  data.frame(x = xyz[, 1L], y = xyz[, 2L], z = xyz[, 3L], u = uv[, 1L], v = uv[, 2L],
    s = rep(h * (0:n), k), start = rep(rays$start, each = n + 1L), angle = rep(a, each = n + 1L),
    group = rep(seq_len(k), each = n + 1L))
}

StatParametricGeodesic3D <- ggplot2::ggproto("StatParametricGeodesic3D", StatGeometry3D,
  required_aes = character(),
  optional_aes = c("x", "y", "z"),
  compute_group = function(data, scales, fun, start, angle = 0, length = 1, n = 200L, args = list()) {
    parametric_geodesics_3d(fun, start, angle, length, n, args)
  }
)

#' Geodesics on parametric surfaces
#'
#' Traces geodesics of the surface `fun(u, v)` of [stat_parametric_3d()]
#' from each `start` parameter pair in each direction `angle`, by
#' integrating the geodesic equation in the parameters. Geodesics are the
#' trajectories of the Hamiltonian H = 1/2 g^ij p_i p_j, with g the first
#' fundamental form; a symplectic, symmetric integrator of order four (the
#' generalised Stoermer-Verlet method composed by Yoshida's triple jump)
#' keeps the speed, and with it the arc length, accurate over long runs.
#' Metric derivatives come from central differences of `fun`.
#'
#' `angle` is measured in the tangent plane at the start, in radians, from
#' the direction of increasing u towards that of increasing v. Each geodesic
#' is followed for arc length `length` in `n` steps and drawn as a path. The
#' computed variables are `u`, `v`, `s` (arc length), `start` (row of
#' `start`) and `angle`. A geodesic that reaches a singular point of the
#' parametrisation, such as a pole of a sphere, stops the layer with an
#' error.
#' @inheritParams stat_parametric_3d
#' @param start Starting parameters, `c(u, v)` or a two-column matrix.
#' @param angle Starting directions in radians.
#' @param length Arc length of each geodesic.
#' @param n Number of integration steps.
#' @param geom Geom drawing the geodesics.
#' @references Hairer, Lubich and Wanner (2006). Geometric Numerical
#'   Integration, 2nd edition. Springer. \doi{10.1007/3-540-30666-8}
#'
#'   Yoshida (1990). Construction of higher order symplectic integrators.
#'   Physics Letters A 150, 262-268. \doi{10.1016/0375-9601(90)90092-3}
#' @return A ggplot2 layer.
#' @examples
#' torus <- function(u, v) list(x = (1 + 0.4 * cos(v)) * cos(u),
#'   y = (1 + 0.4 * cos(v)) * sin(u), z = 0.4 * sin(v))
#' ggplot2::ggplot() +
#'   geom_parametric_3d(fun = torus, n = c(48, 24), closed = TRUE, fill = "grey85") +
#'   stat_parametric_geodesic_3d(ggplot2::aes(colour = ggplot2::after_stat(angle)),
#'     fun = torus, start = c(0, 0), angle = seq(0.2, 1.4, by = 0.3), length = 12) +
#'   coord_3d()
#' @export
stat_parametric_geodesic_3d <- function(mapping = NULL, data = NULL, geom = ggplot2::GeomPath, position = "identity", ...,
                                        fun, start, angle = 0, length = 1, n = 200L, args = list(),
                                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!is.function(fun)) stop("fun must be a function of u and v.", call. = FALSE)
  make_layer_3d(geom, mapping, stats_seed_data_3d(data),
    StatParametricGeodesic3D, position,
    stats_params_3d(list(fun = fun, start = start, angle = angle, length = length, n = n, args = args, na.rm = na.rm),
      list(...)), show.legend, inherit.aes)
}
