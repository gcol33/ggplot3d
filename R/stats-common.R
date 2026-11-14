#' @include layers-surface.R
NULL

utils::globalVariables(c("geometry_isosurface_cpp", "geometry_isosurface_methods_cpp", "geometry_hull_cpp", "geometry_kde_cpp", "density"))

GeomStatsSurface3D <- ggplot2::ggproto("GeomStatsSurface3D", GeomSurface3D,
  optional_aes = c(".opacity", "nx", "ny", "nz", "level", "density", "surface", "se"),
  default_aes = solid_defaults_3d()
)

emit_stats_surface_3d <- function(data, params, panel_params) {
  if (!is.null(data$.opacity)) data$alpha <- data$alpha * data$.opacity
  emit_surface_3d(data, params, panel_params)
}

register_geom_3d(GeomStatsSurface3D, emit_stats_surface_3d, parameters = surface_parameters_3d)

stats_seed_data_3d <- function(data) {
  if (is.null(data)) function(d) if (is.data.frame(d) && nrow(d) > 0L) d else data.frame(.grid = 1) else data
}

stats_params_3d <- function(defaults, extra) {
  defaults[names(extra)] <- extra
  defaults
}

stats_grid_3d <- function(scales, n, axes = c("x", "y")) {
  if (!(length(n) %in% c(1L, length(axes))) || any(!is.finite(n) | n < 2 | n != trunc(n)))
    stop("n must contain integers of at least two, one per grid axis.", call. = FALSE)
  n <- rep_len(n, length(axes))
  out <- lapply(seq_along(axes), function(i) {
    scale <- stats_scale_3d(scales, axes[i])
    if (scale$is_discrete()) stop("Statistical grids require continuous position scales.", call. = FALSE)
    limits <- scale$dimension()
    if (any(!is.finite(limits)) || diff(limits) <= 0) stop("Grid ranges must have positive finite extent.", call. = FALSE)
    seq(limits[1L], limits[2L], length.out = n[i])
  })
  stats::setNames(out, axes)
}

stats_scale_3d <- function(scales, axis) {
  scales[[axis]] %or3d% switch(axis, x = ggplot2::scale_x_continuous(),
    y = ggplot2::scale_y_continuous(), z = scale_z_continuous())
}

StatGeometry3D <- ggplot2::ggproto("StatGeometry3D", ggplot2::Stat,
  dropped_aes = c(".source", "value", "weight")
)

stats_triangles_3d <- function(data) {
  data <- surface_normals_3d(data)
  triangles <- surface_triangles_3d(data, "grid", Inf)
  if (!nrow(triangles)) return(data.frame())
  stats_corners_3d(data, triangles)
}

# Corner table of a triangle mesh: one row per corner with face, corner and
# vertex keys, the face's mean height and face means of the computed
# variables that a fill mapping may read.
stats_corners_3d <- function(data, triangles) {
  out <- data[as.vector(t(triangles)), , drop = FALSE]
  out$.face <- rep(seq_len(nrow(triangles)), each = 3L)
  out$.corner <- rep.int(1:3, nrow(triangles))
  out$.vertex <- as.vector(t(triangles))
  out$z_mean <- rep(rowMeans(matrix(data$z[triangles], ncol = 3L)), each = 3L)
  for (name in intersect(c("density", "ndensity", "count", "se"), names(out)))
    out[[name]] <- rep(rowMeans(matrix(data[[name]][triangles], ncol = 3L)), each = 3L)
  out
}

# Rowbinds corner tables, renumbering faces so that they stay distinct.
stats_bind_faces_3d <- function(pieces) {
  pieces <- Filter(nrow, pieces)
  if (!length(pieces)) return(data.frame())
  shift <- c(0L, cumsum(vapply(pieces, nrow, integer(1)) %/% 3L))
  for (i in seq_along(pieces)) pieces[[i]]$.face <- pieces[[i]]$.face + shift[i]
  do.call(rbind, pieces)
}

# Evaluation mesh of a fitted surface: the full x/y grid (faces = NULL), or
# that grid clipped to the convex hull of the observations.
stats_mesh_3d <- function(scales, n, domain, data) {
  axes <- stats_grid_3d(scales, n)
  switch(domain,
    box = list(points = expand.grid(axes), faces = NULL),
    hull = stats_hull_mesh_3d(axes, cbind(data$x, data$y)))
}

# Triangles of a mesh from stats_mesh_3d() carrying heights z. Faces with a
# missing height are dropped. Grid meshes take central-difference normals,
# clipped meshes area-weighted face normals.
stats_mesh_triangles_3d <- function(mesh, data) {
  if (is.null(mesh$faces)) return(stats_triangles_3d(data))
  faces <- mesh$faces[rowSums(matrix(is.finite(data$z[mesh$faces]), ncol = 3L)) == 3L, , drop = FALSE]
  if (!nrow(faces)) return(data.frame())
  stats_corners_3d(mesh_normals_3d(data, faces), faces)
}

# Vertex normals as the area-weighted sum of the normals of the incident faces.
mesh_normals_3d <- function(data, faces) {
  p <- cbind(data$x, data$y, data$z)
  ab <- p[faces[, 2L], , drop = FALSE] - p[faces[, 1L], , drop = FALSE]
  ac <- p[faces[, 3L], , drop = FALSE] - p[faces[, 1L], , drop = FALSE]
  face <- ab[, c(2L, 3L, 1L), drop = FALSE] * ac[, c(3L, 1L, 2L), drop = FALSE] -
    ab[, c(3L, 1L, 2L), drop = FALSE] * ac[, c(2L, 3L, 1L), drop = FALSE]
  sums <- rowsum(rbind(face, face, face), as.vector(faces))
  normal <- matrix(c(0, 0, 1), nrow(p), 3L, byrow = TRUE)
  normal[as.integer(rownames(sums)), ] <- sums
  size <- sqrt(rowSums(normal^2))
  normal[size == 0, 3L] <- size[size == 0] <- 1
  data[c("nx", "ny", "nz")] <- as.data.frame(normal / size)
  data
}

# Clips the triangulated grid over `axes` to the convex hull of `points`
# (Sutherland-Hodgman against each hull edge) and fans every clipped polygon
# into triangles. Work happens in unit-square coordinates so that tolerances
# do not depend on the axis units; shared vertices are merged afterwards.
stats_hull_mesh_3d <- function(axes, points) {
  points <- points[is.finite(points[, 1L]) & is.finite(points[, 2L]), , drop = FALSE]
  lo <- vapply(axes, min, numeric(1)); span <- vapply(axes, function(a) diff(range(a)), numeric(1))
  unit <- sweep(sweep(unique(points), 2L, lo), 2L, span, "/")
  hull <- if (nrow(unit) >= 3L) unit[rev(grDevices::chull(unit)), , drop = FALSE] else unit
  if (nrow(hull) < 3L || polygon_area_3d(hull) <= 1e-12)
    stop("domain = \"hull\" requires observations spanning a two-dimensional x/y region.", call. = FALSE)
  grid <- expand.grid(lapply(axes, function(a) (a - min(a)) / diff(range(a))))
  triangles <- surface_triangles_3d(cbind(grid, z = 0), "grid", Inf)
  g <- as.matrix(grid); m <- nrow(hull); nxt <- c(seq_len(m)[-1L], 1L); tol <- 1e-10
  side <- outer(g[, 2L], hull[, 2L], "-") * rep(hull[nxt, 1L] - hull[, 1L], each = nrow(g)) -
    outer(g[, 1L], hull[, 1L], "-") * rep(hull[nxt, 2L] - hull[, 2L], each = nrow(g))
  inner <- rowSums(matrix((rowSums(side >= -tol) == m)[triangles], ncol = 3L)) == 3L
  outside <- side < -tol
  separated <- rowSums(outside[triangles[, 1L], , drop = FALSE] & outside[triangles[, 2L], , drop = FALSE] &
    outside[triangles[, 3L], , drop = FALSE]) > 0L
  polygons <- c(lapply(which(inner), function(t) g[triangles[t, ], , drop = FALSE]),
    lapply(which(!inner & !separated), function(t) clip_convex_3d(g[triangles[t, ], , drop = FALSE], hull, tol)))
  corners <- do.call(rbind, lapply(polygons, function(p) {
    k <- nrow(p)
    if (k < 3L) return(NULL)
    p[as.vector(rbind(1L, seq_len(k - 2L) + 1L, seq_len(k - 2L) + 2L)), , drop = FALSE]
  })) %or3d% matrix(numeric(), 0L, 2L)
  ab <- corners[c(FALSE, TRUE, FALSE), , drop = FALSE] - corners[c(TRUE, FALSE, FALSE), , drop = FALSE]
  ac <- corners[c(FALSE, FALSE, TRUE), , drop = FALSE] - corners[c(TRUE, FALSE, FALSE), , drop = FALSE]
  area <- ab[, 1L] * ac[, 2L] - ab[, 2L] * ac[, 1L]
  corners <- corners[rep(area > 1e-14, each = 3L), , drop = FALSE]
  key <- paste(round(corners[, 1L] * 1e9), round(corners[, 2L] * 1e9))
  id <- match(key, unique(key))
  vertices <- corners[!duplicated(key), , drop = FALSE]
  list(points = data.frame(x = lo[1L] + vertices[, 1L] * span[1L], y = lo[2L] + vertices[, 2L] * span[2L]),
    faces = matrix(id, ncol = 3L, byrow = TRUE))
}

polygon_area_3d <- function(p) {
  nxt <- c(seq_len(nrow(p))[-1L], 1L)
  sum(p[, 1L] * p[nxt, 2L] - p[nxt, 1L] * p[, 2L]) / 2
}

# Keeps the part of convex polygon `poly` left of every edge of the
# counterclockwise convex polygon `hull`.
clip_convex_3d <- function(poly, hull, tol) {
  m <- nrow(hull)
  for (k in seq_len(m)) {
    if (nrow(poly) < 3L) break
    a <- hull[k, ]; b <- hull[k %% m + 1L, ]
    s <- (b[1L] - a[1L]) * (poly[, 2L] - a[2L]) - (b[2L] - a[2L]) * (poly[, 1L] - a[1L])
    inside <- s >= -tol
    if (all(inside)) next
    nxt <- c(seq_len(nrow(poly))[-1L], 1L)
    out <- list()
    for (i in seq_len(nrow(poly))) {
      j <- nxt[i]
      if (inside[i]) out[[length(out) + 1L]] <- poly[i, ]
      if (inside[i] != inside[j]) {
        t <- min(max(s[i] / (s[i] - s[j]), 0), 1)
        out[[length(out) + 1L]] <- poly[i, ] + t * (poly[j, ] - poly[i, ])
      }
    }
    poly <- if (length(out)) do.call(rbind, out) else matrix(numeric(), 0L, 2L)
  }
  poly
}

# Adds a default after_stat mapping unless the aesthetic is mapped or set.
stats_default_mapping_3d <- function(mapping, extra, default) {
  aesthetic <- names(default)
  if (!is.null(mapping[[aesthetic]]) || !is.null(extra[[aesthetic]])) return(mapping)
  if (is.null(mapping)) return(default)
  mapping[[aesthetic]] <- default[[aesthetic]]
  mapping
}

surface_normals_3d <- function(data, lattice = surface_lattice_3d(data)) {
  if (max(0L, lattice$i) < 2L || max(0L, lattice$j) < 2L) return(data)
  key <- cbind(lattice$i, lattice$j)
  tangent <- function(axis) {
    size <- max(key[, axis])
    lo <- pmax(1L, key[, axis] - 1L); hi <- pmin(size, key[, axis] + 1L)
    lo_key <- hi_key <- key
    lo_key[, axis] <- lo; hi_key[, axis] <- hi
    vapply(c("x", "y", "z"), function(v) {
      values <- matrix(NA_real_, max(key[, 1L]), max(key[, 2L]))
      values[key] <- data[[v]]
      values[hi_key] - values[lo_key]
    }, numeric(nrow(key)))
  }
  u <- matrix(tangent(1L), ncol = 3L); v <- matrix(tangent(2L), ncol = 3L)
  normal <- cbind(u[, 2L] * v[, 3L] - u[, 3L] * v[, 2L],
                  u[, 3L] * v[, 1L] - u[, 1L] * v[, 3L],
                  u[, 1L] * v[, 2L] - u[, 2L] * v[, 1L])
  normal <- normal * ifelse(normal[, 3L] < 0, -1, 1) / sqrt(rowSums(normal^2))
  data[c("nx", "ny", "nz")] <- as.data.frame(normal)
  data
}

stats_levels_3d <- function(values, levels) {
  if (is.null(levels)) {
    bounds <- range(values, finite = TRUE)
    levels <- pretty(bounds, n = 10L)
    levels <- levels[levels > bounds[1L] & levels < bounds[2L]]
  }
  if (!is.numeric(levels) || !length(levels) || any(!is.finite(levels)))
    stop("levels must be a nonempty finite numeric vector.", call. = FALSE)
  sort(unique(as.double(levels)))
}

isosurface_method_3d <- function(method) {
  methods <- geometry_isosurface_methods_cpp()
  if (!is.character(method) || length(method) < 1L) stop("method must be one of ", paste0('"', methods, '"', collapse = ", "), ".", call. = FALSE)
  match.arg(method[1L], methods)
}

# Axes and values (x fastest) of a complete rectilinear x/y/z grid in data.
stats_scalar_grid_3d <- function(data) {
  axes <- lapply(data[c("x", "y", "z")], function(x) sort(unique(x)))
  dims <- lengths(axes)
  if (any(dims < 2L) || prod(dims) != nrow(data)) stop("A scalar field requires a complete rectilinear x/y/z grid.", call. = FALSE)
  key <- match(data$x, axes$x) + dims[1L] * (match(data$y, axes$y) - 1L + dims[2L] * (match(data$z, axes$z) - 1L))
  if (anyDuplicated(key)) stop("Scalar grid coordinates must be unique.", call. = FALSE)
  list(axes = axes, value = as.double(data$value[order(key)]))
}

# Normal and point of a plane given by sdf_plane(), as six numbers.
plane_coefficients_3d <- function(plane, name = "plane") {
  node <- if (inherits(plane, "ggplot3d_sdf")) sdf_node_3d(plane)
  if (is.null(node) || node$op != "plane") stop(name, " must be a plane made with sdf_plane().", call. = FALSE)
  c(node$params$normal, node$params$point)
}

stats_field_3d <- function(data, levels, caps, method = "cubes", fun = NULL, args = list(), refine = 0L, simplify = NULL, clip = NULL) {
  if (!is.logical(caps) || length(caps) != 1L || is.na(caps)) stop("caps must be TRUE or FALSE.", call. = FALSE)
  method <- isosurface_method_3d(method)
  field <- stats_scalar_grid_3d(data)
  axes <- field$axes; value <- field$value
  f <- if (!is.null(fun)) function_evaluator_3d(fun, args)
  out <- geometry_isosurface_cpp(axes$x, axes$y, axes$z, value, stats_levels_3d(value, levels), caps, method,
    if (!is.null(f)) function_gradient_3d(f, axes), if (!is.null(clip)) plane_coefficients_3d(clip, "clip"))
  isosurface_finish_3d(out, axes, value, f, refine, simplify)
}

# fun as a function of an n x 3 matrix of positions, one value per row. A
# signed distance function also carries its exact gradient.
function_evaluator_3d <- function(fun, args) {
  f <- function(p) {
    v <- do.call(fun, c(list(x = p[, 1L], y = p[, 2L], z = p[, 3L]), args))
    rep_len(as.double(v), nrow(p))
  }
  if (inherits(fun, "ggplot3d_sdf")) attr(f, "gradient") <- function(p) sdf_gradient_3d(fun, p)
  f
}

# Polishes vertices against fun when it is known, decimates to `simplify`
# faces when given, and adds the face depth key.
isosurface_finish_3d <- function(out, axes, value, f, refine, simplify = NULL) {
  if (!is.null(f)) out <- isosurface_polish_3d(out, axes, value, f, refine)
  out$.edge_a <- out$.edge_b <- NULL
  if (!is.null(simplify) && length(out$x)) out <- isosurface_simplify_3d(out, simplify, if (!is.null(f)) function_gradient_3d(f, axes))
  out$z_mean <- rep(colMeans(matrix(out$z, nrow = 3L)), each = 3L)
  out
}

# Parameters t in [0, 1] of the level on segments a + t (b - a) whose
# endpoint values fa, fb (relative to the level) bracket it, after `steps`
# Illinois regula falsi steps (each step evaluates f, relative to the level,
# at the current iterate and moves to the next one).
regula_falsi_3d <- function(a, b, fa, fb, f, steps) {
  ta <- numeric(length(fa)); tb <- rep(1, length(fa)); last <- integer(length(fa))
  secant <- function() ifelse(fb != fa, (ta * fb - tb * fa) / (fb - fa), (ta + tb) / 2)
  t <- secant()
  for (step in seq_len(steps)) {
    ft <- f(a + (b - a) * t)
    ok <- is.finite(ft)
    low <- ok & sign(ft) == sign(fa)
    high <- ok & !low
    # Illinois: halve the value at an endpoint kept for a second step.
    fb[low & last == 1L] <- fb[low & last == 1L] / 2
    fa[high & last == -1L] <- fa[high & last == -1L] / 2
    ta[low] <- t[low]; fa[low] <- ft[low]
    tb[high] <- t[high]; fb[high] <- ft[high]
    last[low] <- 1L; last[high] <- -1L
    t <- secant()
  }
  pmin(pmax(t, 0), 1)
}

# Gradient of a vectorised function of an n x 3 matrix: the exact gradient
# the function carries, or central differences with steps of 1e-5 times each
# axis range, in one call over the six offsets.
function_gradient_3d <- function(f, axes) {
  if (!is.null(attr(f, "gradient"))) return(attr(f, "gradient"))
  h <- vapply(axes, function(a) diff(range(a)), numeric(1)) * 1e-5
  offsets <- rbind(diag(h), -diag(h))
  function(p) {
    probe <- matrix(f(do.call(rbind, lapply(seq_len(6L), function(k) sweep(p, 2L, offsets[k, ], `+`)))), nrow = nrow(p))
    (probe[, 1:3, drop = FALSE] - probe[, 4:6, drop = FALSE]) / rep(2 * h, each = nrow(p))
  }
}

# Moves every vertex that lies on a grid edge onto f = level with `refine`
# Illinois regula falsi steps inside its bracketing edge (each step evaluates f
# at the current iterate and moves to the next one), then takes vertex
# normals from central differences of f at the final positions. Cap faces keep
# their box normals.
isosurface_polish_3d <- function(out, axes, value, f, refine) {
  if (!length(out$x)) return(out)
  ids <- unique(out$.vertex); first <- match(ids, out$.vertex)
  p <- cbind(out$x[first], out$y[first], out$z[first])
  level <- out$level[first]
  ea <- out$.edge_a[first]; eb <- out$.edge_b[first]
  edge <- !is.na(ea) & !is.na(eb) & ea != eb
  if (refine > 0L && any(edge)) {
    n <- lengths(axes)
    grid_point <- function(id) cbind(axes$x[(id - 1L) %% n[1L] + 1L],
      axes$y[((id - 1L) %/% n[1L]) %% n[2L] + 1L], axes$z[(id - 1L) %/% (n[1L] * n[2L]) + 1L])
    a <- grid_point(ea[edge]); b <- grid_point(eb[edge]); target <- level[edge]
    t <- regula_falsi_3d(a, b, value[ea[edge]] - target, value[eb[edge]] - target, function(q) f(q) - target, refine)
    p[edge, ] <- a + (b - a) * t
  }
  gradient <- function_gradient_3d(f, axes)(p)
  size <- sqrt(rowSums(gradient^2))
  good <- is.finite(size) & size > 0
  normal <- gradient / ifelse(good, size, 1)
  row <- match(out$.vertex, ids)
  out$x <- p[row, 1L]; out$y <- p[row, 2L]; out$z <- p[row, 3L]
  surface <- out$cap == 0L & good[row]
  out$nx[surface] <- normal[row[surface], 1L]
  out$ny[surface] <- normal[row[surface], 2L]
  out$nz[surface] <- normal[row[surface], 3L]
  out
}

# Quadric decimation of an extracted isosurface to `target` faces (or a
# fraction of them). Normals are recomputed from the decimated faces, or
# from the gradient of the field function when there is one; cap faces keep
# their face normals.
isosurface_simplify_3d <- function(out, target, gradient = NULL) {
  out <- mesh_decimate(out, target)
  if (!is.null(gradient)) {
    surface <- out$cap == 0L
    g <- gradient(cbind(out$x, out$y, out$z)[surface, , drop = FALSE])
    size <- sqrt(rowSums(g^2)); good <- is.finite(size) & size > 0
    rows <- which(surface)[good]
    out[rows, c("nx", "ny", "nz")] <- g[good, , drop = FALSE] / size[good]
  }
  out
}

stats_bandwidth_3d <- function(data, h, adjust, mass = FALSE) {
  d <- ncol(data)
  if (!(length(adjust) %in% c(1L, d)) || any(!is.finite(adjust) | adjust <= 0)) stop("adjust must be positive and finite.", call. = FALSE)
  if (is.null(h)) {
    if (nrow(data) < 2L) stop("Bandwidth estimation requires at least two observations.", call. = FALSE)
    h <- vapply(data, function(x) {
      b <- stats::bw.nrd(x)
      if (b == 0) b <- stats::bw.nrd0(x)
      if (mass) b * 4 else b * nrow(data)^(1/5 - 1/(d + 4))
    }, numeric(1)) * adjust
  }
  if (!(length(h) %in% c(1L, d)) || any(!is.finite(h) | h <= 0)) stop("h must contain positive finite bandwidths.", call. = FALSE)
  rep_len(h, d) / if (mass) 4 else 1
}

stats_probability_levels_3d <- function(density, axes, levels) {
  if (!inherits(levels, "ggplot3d_probs")) return(stats_levels_3d(density, levels))
  weights <- lapply(axes, function(a) c(diff(a)[1L], head(diff(a), -1L) + tail(diff(a), -1L), tail(diff(a), 1L)) / 2)
  volume <- as.vector(outer(as.vector(outer(weights[[1L]], weights[[2L]])), weights[[3L]]))
  order <- order(density, decreasing = TRUE)
  mass <- cumsum(density[order] * volume[order])
  if (max(mass) <= 0) stop("The density grid contains no probability mass.", call. = FALSE)
  if (max(mass) < max(levels)) warning("The grid contains less mass than the requested probability; expand the position scale limits.", call. = FALSE)
  vapply(as.double(levels), function(p) {
    index <- which(mass >= p)[1L]
    if (is.na(index)) index <- length(order)
    density[order[index]]
  }, numeric(1))
}
