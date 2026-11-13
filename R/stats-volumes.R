StatIsosurface3D <- ggplot2::ggproto("StatIsosurface3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  optional_aes = "value",
  non_missing_aes = character(),
  setup_params = function(data, params) {
    if (is.null(data$value) || !is.numeric(data$value)) stop("An isosurface requires a numeric value aesthetic.", call. = FALSE)
    params
  },
  compute_group = function(data, scales, levels = NULL, caps = FALSE, method = "cubes", simplify = NULL) {
    if (any(vapply(scales[c("x", "y", "z")], function(s) s$is_discrete(), logical(1)))) stop("Isosurfaces require continuous position scales.", call. = FALSE)
    stats_field_3d(data, levels, caps, method, simplify = simplify)
  }
)

StatDensity3D <- ggplot2::ggproto("StatDensity3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  compute_group = function(data, scales, h = NULL, adjust = 1, n = 32L, levels = NULL, caps = FALSE, space = "data", method = "cubes", simplify = NULL) {
    space <- match.arg(space, c("data", "ilr"))
    composition <- if (space == "ilr") composition_space_3d(data) else NULL
    points <- if (is.null(composition)) data[c("x", "y", "z")] else stats::setNames(as.data.frame(composition$coords), c("x", "y", "z"))
    bandwidth <- stats_bandwidth_3d(points, h, adjust)
    axes <- if (is.null(composition)) stats_grid_3d(scales, n, c("x", "y", "z")) else {
      # Log-ratio coordinates are unbounded; the grid covers the data and three bandwidths.
      n <- rep_len(n, 3L)
      stats::setNames(lapply(1:3, function(k) seq(min(points[[k]]) - 3 * bandwidth[k], max(points[[k]]) + 3 * bandwidth[k], length.out = n[k])), c("x", "y", "z"))
    }
    grid <- expand.grid(axes)
    grid$value <- geometry_kde_cpp(as.matrix(points), axes, bandwidth)
    resolved <- stats_probability_levels_3d(grid$value, axes, levels)
    out <- stats_field_3d(grid, resolved, caps, method, simplify = simplify)
    if (!is.null(composition) && length(out$x)) {
      back <- composition$back(cbind(out$x, out$y, out$z))
      out$x <- back[, 1L]; out$y <- back[, 2L]; out$z <- back[, 3L]
      out$z_mean <- rep(colMeans(matrix(out$z, nrow = 3L)), each = 3L)
    }
    out$density <- out$level
    if (inherits(levels, "ggplot3d_probs")) out$probability <- as.double(levels)[match(out$level, resolved)]
    out
  }
)

StatIsofunction3D <- ggplot2::ggproto("StatIsofunction3D", StatGeometry3D,
  required_aes = character(),
  optional_aes = c("x", "y", "z"),
  compute_group = function(data, scales, fun, n = 24L, args = list(), levels = NULL, caps = FALSE, method = "cubes", refine = 2L, simplify = NULL) {
    if (!is.numeric(refine) || length(refine) != 1L || !is.finite(refine) || refine < 0 || refine != trunc(refine))
      stop("refine must be a non-negative integer.", call. = FALSE)
    grid <- expand.grid(stats_grid_3d(scales, n, c("x", "y", "z")))
    value <- do.call(fun, c(grid, args))
    if (!is.numeric(value) || !(length(value) %in% c(1L, nrow(grid)))) stop("fun must return one numeric value per grid point.", call. = FALSE)
    grid$value <- rep_len(value, nrow(grid))
    stats_field_3d(grid, levels, caps, method, fun, args, as.integer(refine), simplify)
  }
)

StatHull3D <- ggplot2::ggproto("StatHull3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  compute_group = function(data, scales, space = "data") {
    if (any(vapply(scales[c("x", "y", "z")], function(s) s$is_discrete(), logical(1)))) stop("Hulls require continuous position scales.", call. = FALSE)
    if (match.arg(space, c("data", "ilr")) == "ilr") {
      composition <- composition_space_3d(data)
      data <- data[composition$rows, , drop = FALSE]
      triangles <- geometry_hull_cpp(composition$coords)
    } else triangles <- geometry_hull_cpp(as.matrix(data[c("x", "y", "z")]))
    if (!nrow(triangles)) return(data.frame())
    rows <- as.vector(t(triangles))
    out <- data[rows, , drop = FALSE]
    out$.face <- rep(seq_len(nrow(triangles)), each = 3L); out$.corner <- rep.int(1:3, nrow(triangles))
    out$.vertex <- rows; out$z_mean <- rep(rowMeans(matrix(data$z[triangles], ncol = 3L)), each = 3L)
    if (!is.null(data$.source)) out$.origin <- data$.source[rows]
    out
  }
)
