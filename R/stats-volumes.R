StatIsosurface3D <- ggplot2::ggproto("StatIsosurface3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  optional_aes = "value",
  non_missing_aes = character(),
  setup_params = function(data, params) {
    if (is.null(data$value) || !is.numeric(data$value)) stop("An isosurface requires a numeric value aesthetic.", call. = FALSE)
    params
  },
  compute_group = function(data, scales, levels = NULL, caps = FALSE) {
    if (any(vapply(scales[c("x", "y", "z")], function(s) s$is_discrete(), logical(1)))) stop("Isosurfaces require continuous position scales.", call. = FALSE)
    stats_field_3d(data, levels, caps)
  }
)

StatDensity3D <- ggplot2::ggproto("StatDensity3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  compute_group = function(data, scales, h = NULL, adjust = 1, n = 32L, levels = NULL, caps = FALSE) {
    axes <- stats_grid_3d(scales, n, c("x", "y", "z"))
    bandwidth <- stats_bandwidth_3d(data[c("x", "y", "z")], h, adjust)
    grid <- expand.grid(axes)
    grid$value <- geometry_kde_cpp(as.matrix(data[c("x", "y", "z")]), axes, bandwidth)
    resolved <- stats_probability_levels_3d(grid$value, axes, levels)
    out <- stats_field_3d(grid, resolved, caps)
    out$density <- out$level
    if (inherits(levels, "ggplot3d_probs")) out$probability <- as.double(levels)[match(out$level, resolved)]
    out
  }
)

StatIsofunction3D <- ggplot2::ggproto("StatIsofunction3D", StatGeometry3D,
  required_aes = character(),
  optional_aes = c("x", "y", "z"),
  compute_group = function(data, scales, fun, n = 24L, args = list(), levels = NULL, caps = FALSE) {
    grid <- expand.grid(stats_grid_3d(scales, n, c("x", "y", "z")))
    value <- do.call(fun, c(grid, args))
    if (!is.numeric(value) || !(length(value) %in% c(1L, nrow(grid)))) stop("fun must return one numeric value per grid point.", call. = FALSE)
    grid$value <- rep_len(value, nrow(grid))
    stats_field_3d(grid, levels, caps)
  }
)

StatHull3D <- ggplot2::ggproto("StatHull3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  compute_group = function(data, scales) {
    if (any(vapply(scales[c("x", "y", "z")], function(s) s$is_discrete(), logical(1)))) stop("Hulls require continuous position scales.", call. = FALSE)
    triangles <- geometry_hull_cpp(as.matrix(data[c("x", "y", "z")]))
    if (!nrow(triangles)) return(data.frame())
    rows <- as.vector(t(triangles))
    out <- data[rows, , drop = FALSE]
    out$.face <- rep(seq_len(nrow(triangles)), each = 3L); out$.corner <- rep.int(1:3, nrow(triangles))
    out$.vertex <- rows; out$z_mean <- rep(rowMeans(matrix(data$z[triangles], ncol = 3L)), each = 3L)
    if (!is.null(data$.source)) out$.origin <- data$.source[rows]
    out
  }
)
