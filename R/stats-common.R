utils::globalVariables(c("geometry_isosurface_cpp", "geometry_hull_cpp", "geometry_kde_cpp", "density"))

GeomStatsSurface3D <- ggplot2::ggproto("GeomStatsSurface3D", GeomSurface3D,
  optional_aes = c(".opacity", "nx", "ny", "nz", "level", "density", "surface"),
  default_aes = solid_defaults_3d()
)

emit_stats_surface_3d <- function(data, params, panel_params) {
  if (!is.null(data$.opacity)) data$alpha <- data$alpha * data$.opacity
  emit_surface_3d(plane_draw_data_3d(data, params, panel_params), params, panel_params)
}

register_geom_3d(GeomStatsSurface3D, emit_stats_surface_3d,
  position_check = function(position, scales) check_position_3d(position, scales))

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
  out <- data[as.vector(t(triangles)), , drop = FALSE]
  out$.face <- rep(seq_len(nrow(triangles)), each = 3L)
  out$.corner <- rep.int(1:3, nrow(triangles))
  out$.vertex <- as.vector(t(triangles))
  out$z_mean <- rep(rowMeans(matrix(data$z[triangles], ncol = 3L)), each = 3L)
  for (name in intersect(c("density", "ndensity", "count"), names(out)))
    out[[name]] <- rep(rowMeans(matrix(data[[name]][triangles], ncol = 3L)), each = 3L)
  out
}

surface_normals_3d <- function(data) {
  axes <- lapply(data[c("x", "y")], function(x) sort(unique(x)))
  if (any(lengths(axes) < 2L)) return(data)
  key <- cbind(match(data$x, axes$x), match(data$y, axes$y))
  z <- matrix(NA_real_, length(axes$x), length(axes$y))
  z[key] <- data$z
  derivative <- function(values, axis) {
    lo <- pmax(1L, seq_along(axis) - 1L)
    hi <- pmin(length(axis), seq_along(axis) + 1L)
    sweep(values[hi, , drop = FALSE] - values[lo, , drop = FALSE],
      1L, axis[hi] - axis[lo], "/")
  }
  normal <- cbind(-derivative(z, axes$x)[key],
    -t(derivative(t(z), axes$y))[key], 1)
  normal <- normal / sqrt(rowSums(normal^2))
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

stats_field_3d <- function(data, levels, caps) {
  if (!is.logical(caps) || length(caps) != 1L || is.na(caps)) stop("caps must be TRUE or FALSE.", call. = FALSE)
  axes <- lapply(data[c("x", "y", "z")], function(x) sort(unique(x)))
  dims <- lengths(axes)
  if (any(dims < 2L) || prod(dims) != nrow(data)) stop("An isosurface requires a complete rectilinear x/y/z grid.", call. = FALSE)
  key <- match(data$x, axes$x) + dims[1L] * (match(data$y, axes$y) - 1L + dims[2L] * (match(data$z, axes$z) - 1L))
  if (anyDuplicated(key)) stop("Scalar grid coordinates must be unique.", call. = FALSE)
  value <- data$value[order(key)]
  out <- geometry_isosurface_cpp(axes$x, axes$y, axes$z, as.double(value), stats_levels_3d(value, levels), caps)
  out$z_mean <- rep(colMeans(matrix(out$z, nrow = 3L)), each = 3L)
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
