utils::globalVariables("sweep_frames_cpp")

spline_types_3d <- c("catmull-rom", "bspline", "natural")

# Finite curve points of one group in row order, without repeated
# consecutive points (nor a last point repeating the first on a closed curve).
curve_points_3d <- function(data, closed) {
  data <- data[is.finite(data$x) & is.finite(data$y) & is.finite(data$z), , drop = FALSE]
  if (nrow(data) > 1L) {
    xyz <- as.matrix(data[c("x", "y", "z")])
    same <- c(FALSE, rowSums(abs(diff(xyz))) == 0)
    if (closed && nrow(data) > 2L && all(xyz[1L, ] == xyz[nrow(xyz), ])) same[nrow(xyz)] <- TRUE
    data <- data[!same, , drop = FALSE]
  }
  if (nrow(data) < 2L + closed) stop("A curve needs at least ", 2L + closed, " distinct points.", call. = FALSE)
  data
}

# n samples of a smooth curve through the rows of xyz ("catmull-rom",
# centripetal; "natural", cubic spline in chord length) or guided by them
# ("bspline", uniform cubic with end points repeated so the curve starts
# and ends on the data), with the fractional row index of every sample. A
# closed curve wraps and its samples do not repeat the first one.
spline_curve_3d <- function(xyz, n, type, closed) {
  m <- nrow(xyz)
  wrap <- function(i) (i - 1L) %% m + 1L
  if (type == "natural") {
    p <- if (closed) rbind(xyz, xyz[1L, ]) else xyz
    t <- c(0, cumsum(sqrt(rowSums(diff(p)^2))))
    s <- seq(0, t[length(t)], length.out = n + closed)[seq_len(n)]
    out <- vapply(1:3, function(a) stats::spline(t, p[, a], method = if (closed) "periodic" else "natural", xout = s)$y, numeric(n))
    return(list(xyz = matrix(out, ncol = 3L), index = stats::approx(t, seq_along(t), s)$y))
  }
  segments <- if (closed) m else m - 1L
  s <- seq(0, segments, length.out = n + closed)[seq_len(n)]
  k <- pmin(floor(s), segments - 1L); u <- s - k
  if (type == "bspline") {
    q <- if (closed) xyz[wrap(seq.int(0L, m + 2L)), , drop = FALSE] else xyz[c(1L, 1L, seq_len(m), m, m), , drop = FALSE]
    segments_b <- nrow(q) - 3L
    sb <- seq(0, segments_b, length.out = n + closed)[seq_len(n)]
    kb <- pmin(floor(sb), segments_b - 1L); ub <- sb - kb
    w <- cbind((1 - ub)^3, 3 * ub^3 - 6 * ub^2 + 4, -3 * ub^3 + 3 * ub^2 + 3 * ub + 1, ub^3) / 6
    out <- w[, 1L] * q[kb + 1L, ] + w[, 2L] * q[kb + 2L, ] + w[, 3L] * q[kb + 3L, ] + w[, 4L] * q[kb + 4L, ]
    return(list(xyz = out, index = 1 + s))
  }
  p <- if (closed) xyz[wrap(seq.int(0L, m + 2L)), , drop = FALSE] else
    rbind(2 * xyz[1L, ] - xyz[2L, ], xyz, 2 * xyz[m, ] - xyz[m - 1L, ])
  knots <- c(0, cumsum(pmax(sqrt(sqrt(rowSums(diff(p)^2))), 1e-12)))
  i <- k + 2L
  t0 <- knots[i - 1L]; t1 <- knots[i]; t2 <- knots[i + 1L]; t3 <- knots[i + 2L]
  t <- t1 + u * (t2 - t1)
  mix <- function(a, b, ta, tb) ((tb - t) * a + (t - ta) * b) / (tb - ta)
  a1 <- mix(p[i - 1L, ], p[i, ], t0, t1); a2 <- mix(p[i, ], p[i + 1L, ], t1, t2); a3 <- mix(p[i + 1L, ], p[i + 2L, ], t2, t3)
  out <- mix(mix(a1, a2, t0, t2), mix(a2, a3, t1, t3), t1, t2)
  list(xyz = out, index = 1 + s)
}

# Rows of data at fractional row positions: double columns are linearly
# interpolated (wrapping on a closed curve), other columns take the row at
# or before the position.
curve_resample_3d <- function(data, index, closed) {
  m <- nrow(data); rows <- seq_len(m)
  base <- pmin(floor(index), m); frac <- index - base
  after <- if (closed) base %% m + 1L else pmin(base + 1L, m)
  out <- data[base, , drop = FALSE]
  for (name in names(data)) if (is.double(data[[name]]))
    out[[name]] <- (1 - frac) * data[[name]][base] + frac * data[[name]][after]
  rownames(out) <- NULL
  out
}

# Triangulated surface on a grid of samples (first direction fastest),
# welded where samples coincide, with angle-weighted normals split at
# creases above 60 degrees. ring_row gives the data row behind every sample
# row (ring, second grid direction). A face spans two rings: double columns
# of data and each entry of ring_values (one value per ring) take the mean
# of the two rings' values, other columns the value of the lower ring.
grid_surface_3d <- function(samples, dims, closed, ring_row, data, ring_values = list(), drop = character()) {
  grid <- parametric_grid_cpp(as.integer(dims), closed, c(FALSE, FALSE), samples, c(0, 1), c(0, 1))
  if (!nrow(grid$triangles)) return(data.frame())
  welded <- samples[grid$sample, , drop = FALSE]
  res <- mesh_process_cpp(welded, grid$triangles, seq_len(nrow(grid$triangles)), matrix(0, nrow(welded), 0L),
    "normals", list(crease = 60))
  nt <- nrow(res$triangles); v <- as.vector(t(res$triangles))
  ring <- matrix((grid$sample[res$source[v]] - 1L) %/% dims[1L] + 1L, nrow = 3L)
  lo <- rep(pmin(ring[1L, ], ring[2L, ], ring[3L, ]), each = 3L)
  hi <- rep(pmax(ring[1L, ], ring[2L, ], ring[3L, ]), each = 3L)
  between <- function(x) (x[lo] + x[hi]) / 2
  out <- data.frame(x = res$xyz[v, 1L], y = res$xyz[v, 2L], z = res$xyz[v, 3L],
    nx = res$corner[, 1L], ny = res$corner[, 2L], nz = res$corner[, 3L])
  for (name in setdiff(names(data), c(names(out), drop))) {
    x <- data[[name]][ring_row]
    out[[name]] <- if (is.double(x)) between(x) else x[lo]
  }
  for (name in names(ring_values)) out[[name]] <- between(ring_values[[name]])
  out$.face <- rep(seq_len(nt), each = 3L); out$.corner <- rep.int(1:3, nt); out$.vertex <- v
  out$z_mean <- rep(colMeans(matrix(out$z, nrow = 3L)), each = 3L)
  out
}

sweep_profile_3d <- function(profile, sides, closed) {
  if (is.null(profile)) {
    sides <- mesh_count_3d(sides, "sides", 3L)
    a <- 2 * pi * (seq_len(sides) - 1L) / sides
    return(list(points = cbind(cos(a), sin(a)), closed = TRUE))
  }
  p <- if (is.data.frame(profile)) as.matrix(profile[1:2]) else as.matrix(profile)
  if (!is.numeric(p) || ncol(p) != 2L || nrow(p) < 2L || any(!is.finite(p))) stop("profile must be a two-column matrix of at least two finite points.", call. = FALSE)
  if (nrow(p) > 2L && all(p[1L, ] == p[nrow(p), ])) p <- p[-nrow(p), , drop = FALSE]
  closed <- closed %or3d% (nrow(p) > 2L)
  if (closed) {
    if (nrow(p) < 3L) stop("A closed profile needs at least three points.", call. = FALSE)
    nxt <- p[c(seq.int(2L, nrow(p)), 1L), , drop = FALSE]
    if (sum(p[, 1L] * nxt[, 2L] - nxt[, 1L] * p[, 2L]) < 0) p <- p[rev(seq_len(nrow(p))), , drop = FALSE]
  }
  list(points = unname(p), closed = closed)
}

# Swept surface of profile along the curve in data: the profile, scaled by
# size and turned by twist (radians) about the tangent, is placed in the
# plane of the rotation-minimising normal r and binormal s at every point.
# Caps on an open curve with a closed profile: "flat" closes each end with
# the profile shrunk to its centre point, "round" with profiles shrinking
# along a quarter circle whose radius is the profile's largest distance
# from its origin.
sweep_surface_3d <- function(data, size, twist, profile, closed, cap, normal) {
  xyz <- as.matrix(data[c("x", "y", "z")])
  n <- nrow(xyz)
  f <- sweep_frames_cpp(xyz, closed, as.double(normal %or3d% c(0, 0, 0)))
  ring <- list(centre = xyz, size = size, twist = twist, r = f$normal, s = f$binormal, row = seq_len(n), arc = f$arc)
  if (!closed && profile$closed && cap != "none") {
    k <- if (cap == "flat") 1L else max(2L, ceiling(nrow(profile$points) / 4L))
    reach <- max(sqrt(rowSums(profile$points^2)))
    phi <- if (cap == "flat") pi / 2 else seq_len(k) / k * pi / 2
    end_ring <- function(i, direction) {
      t <- f$tangent[i, ] * direction
      list(centre = if (cap == "flat") xyz[rep(i, k), , drop = FALSE] else
             sweep(outer(reach * size[i] * sin(phi), t), 2L, xyz[i, ], `+`),
           size = c(size[i] * cos(phi[-k]), 0), twist = rep(twist[i], k),
           r = f$normal[rep(i, k), , drop = FALSE], s = f$binormal[rep(i, k), , drop = FALSE],
           row = rep(i, k), arc = rep(f$arc[i], k))
    }
    head <- end_ring(1L, -1); tail <- end_ring(n, 1)
    head <- lapply(head, function(x) if (is.matrix(x)) x[rev(seq_len(k)), , drop = FALSE] else rev(x))
    ring <- Map(function(first, body, last) if (is.matrix(body)) rbind(first, body, last) else c(first, body, last), head, ring, tail)
  }
  p <- profile$points; m <- nrow(p); q <- length(ring$size)
  a <- outer(p[, 1L], cos(ring$twist)) - outer(p[, 2L], sin(ring$twist))
  b <- outer(p[, 1L], sin(ring$twist)) + outer(p[, 2L], cos(ring$twist))
  a <- sweep(a, 2L, ring$size, `*`); b <- sweep(b, 2L, ring$size, `*`)
  samples <- vapply(1:3, function(k) as.vector(sweep(a, 2L, ring$r[, k], `*`) + sweep(b, 2L, ring$s[, k], `*`) +
    rep(ring$centre[, k], each = m)), numeric(m * q))
  grid_surface_3d(matrix(samples, ncol = 3L), c(m, q), c(profile$closed, closed), ring$row, data,
    list(arc_length = ring$arc), drop = c("radius", "width", "twist"))
}

# Cross-section sizes in millimetres where neither the aesthetic nor the
# parameter gives one: tube radius and ribbon width.
sweep_default_mm_3d <- c(tube = 1, ribbon = 4)

# Centre line of a sweep whose cross section is sized in millimetres: the
# curve points with their size (.sweep_size, mm) and twist (.sweep_twist,
# radians), swept by emit_sweep_3d() once the drawing camera and panel size
# are known. arc_length is measured along the curve in data units and z_mean
# is the height of the curve point.
sweep_centreline_3d <- function(data, size, twist, closed) {
  xyz <- as.matrix(data[c("x", "y", "z")])
  data$.sweep_size <- size
  data$.sweep_twist <- twist
  data$arc_length <- c(0, cumsum(sqrt(rowSums(diff(xyz)^2))))
  data$z_mean <- data$z
  data[setdiff(names(data), c("radius", "width", "twist"))]
}

StatSweep3D <- ggplot2::ggproto("StatSweep3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  optional_aes = c("radius", "width", "twist"),
  dropped_aes = c(StatGeometry3D$dropped_aes, "radius", "width", "twist"),
  compute_group = function(data, scales, profile = NULL, sides = 16L, radius = NULL, width = NULL,
                           cap = "round", normal = NULL, closed = FALSE, spline = NULL, n = 100L, profile_closed = NULL,
                           radius.unit = "data") {
    if (any(vapply(c("x", "y", "z"), function(a) stats_scale_3d(scales, a)$is_discrete(), logical(1))))
      stop("Swept surfaces require continuous position scales.", call. = FALSE)
    if (!is.logical(closed) || length(closed) != 1L || is.na(closed)) stop("closed must be TRUE or FALSE.", call. = FALSE)
    cap <- match.arg(cap, c("round", "flat", "none"))
    if (!is.null(normal) && (!is.numeric(normal) || length(normal) != 3L || any(!is.finite(normal)))) stop("normal must be three finite numbers.", call. = FALSE)
    data <- curve_points_3d(data, closed)
    if (!is.null(spline)) {
      curve <- spline_curve_3d(as.matrix(data[c("x", "y", "z")]), mesh_count_3d(n, "n", 2L), match.arg(spline, spline_types_3d), closed)
      data <- curve_resample_3d(data, curve$index, closed)
      data[c("x", "y", "z")] <- as.data.frame(curve$xyz)
      data <- curve_points_3d(data, closed)
    }
    shape <- sweep_profile_3d(profile, sides, profile_closed)
    millimetres <- check_unit_3d(radius.unit, "radius.unit") == "mm"
    default <- if (millimetres) sweep_default_mm_3d[[if (shape$closed) "tube" else "ribbon"]] else
      (if (shape$closed) 0.02 else 0.08) * sqrt(sum(vapply(c("x", "y", "z"), function(a) diff(stats_scale_3d(scales, a)$dimension())^2, numeric(1))))
    fallback <- radius %or3d% width %or3d% default
    size <- data$radius %or3d% data$width %or3d% rep(fallback, nrow(data))
    if (any(!is.finite(size) | size < 0)) stop("Radius and width must be finite and nonnegative.", call. = FALSE)
    twist <- (data$twist %or3d% rep(0, nrow(data))) * pi / 180
    if (millimetres) return(sweep_centreline_3d(data, as.double(size), twist, closed))
    sweep_surface_3d(data, as.double(size), twist, shape, closed, cap, normal)
  }
)

StatSpline3D <- ggplot2::ggproto("StatSpline3D", ggplot2::Stat,
  required_aes = c("x", "y", "z"),
  compute_group = function(data, scales, n = 100L, type = "catmull-rom", closed = FALSE) {
    if (!is.logical(closed) || length(closed) != 1L || is.na(closed)) stop("closed must be TRUE or FALSE.", call. = FALSE)
    data <- curve_points_3d(data, closed)
    curve <- spline_curve_3d(as.matrix(data[c("x", "y", "z")]), mesh_count_3d(n, "n", 2L), match.arg(type, spline_types_3d), closed)
    out <- curve_resample_3d(data, curve$index, closed)
    out[c("x", "y", "z")] <- as.data.frame(curve$xyz)
    if (closed) out <- out[c(seq_len(nrow(out)), 1L), , drop = FALSE]
    rownames(out) <- NULL
    out
  }
)

StatRevolve3D <- ggplot2::ggproto("StatRevolve3D", StatGeometry3D,
  required_aes = "r",
  dropped_aes = c(StatGeometry3D$dropped_aes, "r"),
  optional_aes = c("x", "y", "z"),
  compute_group = function(data, scales, axis = "z", n = 48L, closed = FALSE, angle = c(0, 360), centre = c(0, 0)) {
    axis <- match.arg(axis, c("x", "y", "z"))
    if (is.null(data[[axis]])) stop("A surface of revolution needs r and the ", axis, " aesthetic.", call. = FALSE)
    if (!is.numeric(angle) || length(angle) != 2L || any(!is.finite(angle)) || diff(angle) <= 0 || diff(angle) > 360)
      stop("angle must be an increasing range of at most 360 degrees.", call. = FALSE)
    if (!is.numeric(centre) || length(centre) != 2L || any(!is.finite(centre))) stop("centre must be two finite numbers.", call. = FALSE)
    data <- data[is.finite(data$r) & is.finite(data[[axis]]), , drop = FALSE]
    if (any(data$r < 0)) stop("r must be nonnegative.", call. = FALSE)
    if (nrow(data) < 2L) stop("A profile needs at least two points.", call. = FALSE)
    full <- diff(angle) >= 360 - 1e-9
    n <- mesh_count_3d(n, "n", 3L)
    theta <- (angle[1L] + if (full) 360 * (seq_len(n) - 1L) / n else diff(angle) * (seq_len(n) - 1L) / (n - 1L)) * pi / 180
    m <- nrow(data)
    plane <- switch(axis, z = c("x", "y"), x = c("y", "z"), y = c("z", "x"))
    samples <- matrix(0, n * m, 3L, dimnames = list(NULL, c("x", "y", "z")))
    samples[, plane[1L]] <- centre[1L] + as.vector(outer(cos(theta), data$r))
    samples[, plane[2L]] <- centre[2L] + as.vector(outer(sin(theta), data$r))
    samples[, axis] <- rep(data[[axis]], each = n)
    grid_surface_3d(samples, c(n, m), c(full, closed), seq_len(m), data, drop = c("r", "x", "y", "z"))
  }
)
