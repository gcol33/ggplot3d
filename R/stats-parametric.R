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

# Unit normal r_u x r_v, Gaussian and mean curvature from the first and second
# fundamental forms, with derivatives by central differences in one call to
# fun over a nine-point stencil.
parametric_frame_3d <- function(evaluate, uv, h) {
  k <- nrow(uv)
  if (!k) return(list(normal = matrix(0, 0, 3), gaussian = numeric(), mean = numeric()))
  offsets <- rbind(c(0, 0), c(1, 0), c(-1, 0), c(0, 1), c(0, -1), c(1, 1), c(1, -1), c(-1, 1), c(-1, -1))
  stencil <- do.call(rbind, lapply(seq_len(9L), function(s) sweep(uv, 2L, offsets[s, ] * h, `+`)))
  r <- evaluate(stencil)
  at <- function(s) r[(s - 1L) * k + seq_len(k), , drop = FALSE]
  ru <- (at(2) - at(3)) / (2 * h[1L]); rv <- (at(4) - at(5)) / (2 * h[2L])
  ruu <- (at(2) - 2 * at(1) + at(3)) / h[1L]^2; rvv <- (at(4) - 2 * at(1) + at(5)) / h[2L]^2
  ruv <- (at(6) - at(7) - at(8) + at(9)) / (4 * h[1L] * h[2L])
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
