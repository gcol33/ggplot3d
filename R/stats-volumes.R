StatIsosurface3D <- ggplot2::ggproto("StatIsosurface3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  optional_aes = "value",
  non_missing_aes = character(),
  setup_params = function(data, params) {
    if (is.null(data$value) || !is.numeric(data$value)) stop("An isosurface requires a numeric value aesthetic.", call. = FALSE)
    params
  },
  compute_group = function(data, scales, levels = NULL, caps = FALSE, method = "cubes", simplify = NULL, clip = NULL) {
    if (any(vapply(scales[c("x", "y", "z")], function(s) s$is_discrete(), logical(1)))) stop("Isosurfaces require continuous position scales.", call. = FALSE)
    stats_field_3d(data, levels, caps, method, simplify = simplify, clip = clip)
  }
)

# Gaussian product-kernel density of x, y, z (or of their log-ratio
# coordinates) on a grid: the grid as a data frame with value, and the axes.
stats_density_grid_3d <- function(data, scales, h, adjust, n, composition = NULL) {
  points <- if (is.null(composition)) data[c("x", "y", "z")] else stats::setNames(as.data.frame(composition$coords), c("x", "y", "z"))
  bandwidth <- stats_bandwidth_3d(points, h, adjust)
  axes <- if (is.null(composition)) stats_grid_3d(scales, n, c("x", "y", "z")) else {
    # Log-ratio coordinates are unbounded; the grid covers the data and three bandwidths.
    n <- rep_len(n, 3L)
    stats::setNames(lapply(1:3, function(k) seq(min(points[[k]]) - 3 * bandwidth[k], max(points[[k]]) + 3 * bandwidth[k], length.out = n[k])), c("x", "y", "z"))
  }
  grid <- expand.grid(axes)
  grid$value <- geometry_kde_cpp(as.matrix(points), axes, bandwidth)
  list(grid = grid, axes = axes)
}

StatDensity3D <- ggplot2::ggproto("StatDensity3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  compute_group = function(data, scales, h = NULL, adjust = 1, n = 32L, levels = NULL, caps = FALSE, space = "data", method = "cubes",
                           simplify = NULL, clip = NULL) {
    space <- match.arg(space, c("data", "ilr"))
    if (space == "ilr" && !is.null(clip)) stop('clip requires space = "data".', call. = FALSE)
    composition <- if (space == "ilr") composition_space_3d(data) else NULL
    density <- stats_density_grid_3d(data, scales, h, adjust, n, composition)
    grid <- density$grid; axes <- density$axes
    resolved <- stats_probability_levels_3d(grid$value, axes, levels)
    out <- stats_field_3d(grid, resolved, caps, method, simplify = simplify, clip = clip)
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
  compute_group = function(data, scales, fun, n = 24L, args = list(), levels = NULL, caps = FALSE, method = "cubes", refine = 2L,
                           adaptive = FALSE, depth = c(4L, 8L), tol = 0.002, simplify = NULL, clip = NULL) {
    if (!is.numeric(refine) || length(refine) != 1L || !is.finite(refine) || refine < 0 || refine != trunc(refine))
      stop("refine must be a non-negative integer.", call. = FALSE)
    if (isTRUE(adaptive)) {
      if (!is.null(clip)) stop("clip requires grid extraction (adaptive = FALSE).", call. = FALSE)
      return(stats_adaptive_field_3d(fun, args, scales, levels, caps, method, depth, tol, as.integer(refine), simplify))
    }
    grid <- stats_function_grid_3d(fun, args, scales, n)
    stats_field_3d(grid, levels, caps, method, fun, args, as.integer(refine), simplify, clip)
  }
)

# fun evaluated on a grid over the position scales, as a data frame with value.
stats_function_grid_3d <- function(fun, args, scales, n) {
  grid <- expand.grid(stats_grid_3d(scales, n, c("x", "y", "z")))
  value <- do.call(fun, c(grid, args))
  if (!is.numeric(value) || !(length(value) %in% c(1L, nrow(grid)))) stop("fun must return one numeric value per grid point.", call. = FALSE)
  grid$value <- rep_len(as.double(value), nrow(grid))
  grid
}

# The oblique slice planes: NULL, one sdf_plane() or a list of them.
slice_plane_list_3d <- function(slice) {
  if (is.null(slice)) return(list())
  if (inherits(slice, "ggplot3d_sdf")) return(list(slice))
  if (!is.list(slice)) stop("slice must be a plane made with sdf_plane() or a list of them.", call. = FALSE)
  slice
}

# Planes of the requested slices as rows of normal and point, with labels.
# With no slice given, one plane per axis passes through the grid centre.
slice_planes_3d <- function(axes, xslice, yslice, zslice, slice) {
  if (all(vapply(list(xslice, yslice, zslice, slice), is.null, logical(1)))) {
    centre <- vapply(axes, function(a) mean(range(a)), numeric(1))
    xslice <- centre[[1L]]; yslice <- centre[[2L]]; zslice <- centre[[3L]]
  }
  planes <- list(); labels <- character()
  for (a in 1:3) {
    at <- list(xslice, yslice, zslice)[[a]]
    if (is.null(at)) next
    if (!is.numeric(at) || any(!is.finite(at))) stop(c("x", "y", "z")[a], "slice must contain finite numbers.", call. = FALSE)
    for (v in as.double(at)) {
      normal <- c(0, 0, 0); normal[a] <- 1; point <- c(0, 0, 0); point[a] <- v
      planes[[length(planes) + 1L]] <- c(normal, point)
      labels <- c(labels, paste(c("x", "y", "z")[a], "=", format(v)))
    }
  }
  for (k in seq_along(slice_plane_list_3d(slice))) {
    planes[[length(planes) + 1L]] <- plane_coefficients_3d(slice_plane_list_3d(slice)[[k]], "slice")
    labels <- c(labels, paste("plane", k))
  }
  list(planes = planes, labels = labels)
}

# Sections of a gridded field by planes: triangles with vertex values
# (.value), face means (value), slice labels and unique face and vertex ids.
slice_field_3d <- function(field, xslice, yslice, zslice, slice, n) {
  axes <- field$axes
  n <- n %or3d% max(lengths(axes))
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 2 || n != trunc(n)) stop("n must be an integer of at least two.", call. = FALSE)
  spec <- slice_planes_3d(axes, xslice, yslice, zslice, slice)
  faces <- 0L; vertices <- 0L
  pieces <- lapply(seq_along(spec$planes), function(k) {
    out <- geometry_slice_cpp(axes$x, axes$y, axes$z, field$value, spec$planes[[k]], as.integer(n))
    if (!nrow(out)) return(NULL)
    out$.face <- out$.face + faces; out$.vertex <- out$.vertex + vertices
    faces <<- max(out$.face); vertices <<- max(out$.vertex)
    out$slice <- spec$labels[k]
    out
  })
  out <- do.call(rbind, pieces)
  if (is.null(out)) return(data.frame())
  out$slice <- factor(out$slice, levels = unique(spec$labels))
  names(out)[names(out) == "value"] <- ".value"
  out$value <- rep(colMeans(matrix(out$.value, nrow = 3L)), each = 3L)
  out$z_mean <- rep(colMeans(matrix(out$z, nrow = 3L)), each = 3L)
  out
}

StatSlice3D <- ggplot2::ggproto("StatSlice3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  optional_aes = "value",
  non_missing_aes = character(),
  setup_params = function(data, params) {
    if (is.null(data$value) || !is.numeric(data$value)) stop("A slice requires a numeric value aesthetic.", call. = FALSE)
    params
  },
  default_aes = ggplot2::aes(fill = ggplot2::after_stat(value)),
  compute_group = function(data, scales, xslice = NULL, yslice = NULL, zslice = NULL, slice = NULL, n = NULL) {
    if (any(vapply(scales[c("x", "y", "z")], function(s) s$is_discrete(), logical(1)))) stop("Slices require continuous position scales.", call. = FALSE)
    slice_field_3d(stats_scalar_grid_3d(data), xslice, yslice, zslice, slice, n)
  }
)

StatDensitySlice3D <- ggplot2::ggproto("StatDensitySlice3D", StatGeometry3D,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(fill = ggplot2::after_stat(density)),
  compute_group = function(data, scales, h = NULL, adjust = 1, n = 32L, xslice = NULL, yslice = NULL, zslice = NULL, slice = NULL) {
    density <- stats_density_grid_3d(data, scales, h, adjust, n)
    out <- slice_field_3d(stats_scalar_grid_3d(density$grid), xslice, yslice, zslice, slice, max(rep_len(n, 3L)))
    if (nrow(out)) out$density <- out$value
    out
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
