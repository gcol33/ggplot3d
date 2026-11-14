# Spaces operate on row matrices of canonical points. A chart supplies input
# coordinates; a model supplies display coordinates in its own axis units.
Space3D <- ggplot2::ggproto("Space3D", NULL,
  name = "space",
  charts = character(),
  models = character(),
  default_model = NULL,
  identity = FALSE,
  to_canonical = function(data, chart) stop("Space must implement to_canonical().", call. = FALSE),
  from_canonical = function(X, model) stop("Space must implement from_canonical().", call. = FALSE),
  distance = function(X, Y) stop("Space must implement distance().", call. = FALSE),
  geodesic = function(X, Y, s) stop("Space must implement geodesic().", call. = FALSE),
  act = function(g, X) stop("Space must implement act() for an isometry.", call. = FALSE),
  chart_is_projective = function(model) FALSE,
  model_ranges = function(model, panel_params) panel_params$ranges
)

SpaceEuclidean <- ggplot2::ggproto("SpaceEuclidean", Space3D,
  name = "euclidean", charts = "cartesian", models = "cartesian",
  default_model = "cartesian", identity = TRUE,
  to_canonical = function(data, chart) as.matrix(data[c("x", "y", "z")]),
  from_canonical = function(X, model) X,
  distance = function(X, Y) sqrt(rowSums((X - Y)^2)),
  geodesic = function(X, Y, s) segment_points_3d(X, Y, s),
  act = function(g, X) {
    if (!is.matrix(g) || !is.numeric(g) || !identical(dim(g), c(4L, 4L)) || any(!is.finite(g)) ||
        max(abs(g[4L, ] - c(0, 0, 0, 1))) > 1e-12 ||
        max(abs(crossprod(g[1:3, 1:3]) - diag(3))) > 1e-12) {
      stop("A Euclidean isometry must be a rigid 4 by 4 matrix.", call. = FALSE)
    }
    (cbind(X, 1) %*% t(g))[, 1:3, drop = FALSE]
  },
  chart_is_projective = function(model) TRUE
)

space_spec_3d <- function(space, model, input, isometry, tolerance, max_depth) {
  if (identical(space, "euclidean")) space <- SpaceEuclidean
  if (!inherits(space, "Space3D")) stop("space must be 'euclidean' or a Space3D prototype.", call. = FALSE)
  model <- model %or3d% space$default_model
  input <- input %or3d% model
  if (!is.character(model) || length(model) != 1L || is.na(model) || !model %in% space$models) stop("Unknown space model.", call. = FALSE)
  if (!is.character(input) || length(input) != 1L || is.na(input) || !input %in% space$charts) stop("Unknown space input chart.", call. = FALSE)
  if (!is.numeric(tolerance) || length(tolerance) != 1L || !is.finite(tolerance) || tolerance <= 0) stop("munch_tolerance must be positive and finite.", call. = FALSE)
  if (!is.numeric(max_depth) || length(max_depth) != 1L || !is.finite(max_depth) || max_depth < 0 || max_depth > 12 || max_depth != trunc(max_depth)) stop("munch_max_depth must be an integer from 0 to 12.", call. = FALSE)
  list(space = space, model = model, input = input, isometry = isometry,
    tolerance = tolerance, max_depth = as.integer(max_depth),
    identity = identical(space, SpaceEuclidean) && is.null(isometry))
}

space_matrix_3d <- function(X, n, operation) {
  if (!is.matrix(X) || !is.numeric(X) || nrow(X) != n || !ncol(X) || any(!is.finite(X))) {
    stop("Space ", operation, " must return a finite numeric matrix with one row per point.", call. = FALSE)
  }
  X
}

space_canonical_3d <- function(data, context) {
  X <- space_matrix_3d(context$space$to_canonical(data, context$input), nrow(data), "to_canonical()")
  if (!is.null(context$isometry)) X <- space_matrix_3d(context$space$act(context$isometry, X), nrow(X), "act()")
  X
}

# The sole conversion from a space's model coordinates to packet coordinates.
# Homogeneous coordinates are normalised before any perspective division.
space_packet_coordinates_3d <- function(X, context, homogeneous = FALSE) {
  xyz <- space_matrix_3d(context$space$from_canonical(X, context$model), nrow(X), "from_canonical()")
  if (!ncol(xyz) %in% c(3L, 4L)) stop("A space model must return x, y, z and optional homogeneous w.", call. = FALSE)
  w <- if (ncol(xyz) == 4L) xyz[, 4L] else rep(1, nrow(xyz))
  if (homogeneous) {
    if (!exists("normalise_homogeneous_3d", mode = "function")) stop("Homogeneous space models require scene schema 2 (#16).", call. = FALSE)
    normalise <- get("normalise_homogeneous_3d", mode = "function")
    return(normalise(xyz[, 1L], xyz[, 2L], xyz[, 3L], w, context$ranges))
  }
  if (any(w != 1)) stop("Homogeneous space models and ideal points require scene schema 2 (#16).", call. = FALSE)
  out <- lapply(seq_len(3L), function(j) as.double((xyz[, j] - context$ranges[[j]][1L]) / diff(context$ranges[[j]])))
  names(out) <- c("x", "y", "z")
  out
}

bind_space_3d <- function(spec, panel_params) {
  if (is.null(spec) || spec$identity) return(NULL)
  spec$ranges <- spec$space$model_ranges(spec$model, panel_params)
  if (!is.list(spec$ranges) || !setequal(names(spec$ranges), c("x", "y", "z")) ||
      !all(vapply(spec$ranges, function(x) is.numeric(x) && length(x) == 2L && all(is.finite(x)) && is.finite(diff(x)) && diff(x) > 0, logical(1)))) {
    stop("Space model_ranges() must return finite increasing x, y, z ranges.", call. = FALSE)
  }
  spec$ranges <- spec$ranges[c("x", "y", "z")]
  spec$input_ranges <- panel_params$ranges
  spec$frame <- frame_of_3d(panel_params)$space_frame %or3d% frame_of_3d(panel_params)
  spec$camera <- panel_params$camera
  spec$viewport <- emit_viewport_3d(panel_params)
  spec
}

space_input_3d <- function(data, context) {
  for (j in c("x", "y", "z")) data[[j]] <- context$input_ranges[[j]][1L] + data[[j]] * diff(context$input_ranges[[j]])
  data
}

space_subdivision_3d <- function(context) {
  basis <- frame_basis_3d(context$camera, context$viewport)
  # Edge snapping and interior interpolation share the screen-error budget.
  list(tolerance = context$tolerance / 2, strict = TRUE, max_level = context$max_depth,
    projective = isTRUE(context$space$chart_is_projective(context$model)) && isTRUE(context$frame$linear),
    interpolate = function(X, Y, s) {
      out <- space_matrix_3d(context$space$geodesic(X, Y, s), nrow(X), "geodesic()")
      if (ncol(out) != ncol(X)) stop("Space geodesic() changed canonical dimension.", call. = FALSE)
      out
    },
    map = function(X) context$frame$map(do.call(cbind, space_packet_coordinates_3d(X, context))),
    metric = function(xyz) frame_positions_3d(xyz, context$camera, basis)[, 1:2, drop = FALSE])
}

space_packet_3d <- function(packet, context) {
  if (!length(packet$vertices$x)) return(packet)
  points <- space_canonical_3d(space_input_3d(as.data.frame(packet$vertices), context), context)
  sampler <- space_subdivision_3d(context)
  sampler$linear <- sampler$projective
  packet <- munch_packet_3d(sampler, packet, clip = FALSE, coordinates = points)
  attr(packet, "parent") <- NULL
  attr(packet, "scale") <- NULL
  attr(packet, "space_mapped") <- TRUE
  attr(packet, "model_space") <- TRUE
  packet
}

# Geodesic error uses the fitted drawing camera and the panel's final aspect.
resolve_space_packets_3d <- function(packets, camera, viewport) {
  lapply(packets, function(packet) {
    recipe <- attr(packet, "space_recipe")
    if (is.null(recipe)) return(packet)
    recipe$panel_params$camera <- camera
    recipe$panel_params$viewport <- viewport
    recipe$panel_params$space_context$camera <- camera
    recipe$panel_params$space_context$viewport <- viewport
    do.call(emit_scene_3d, recipe)
  })
}

# Numeric aesthetics follow geodesic parameters. Discrete values and source
# identities belong to the first corner; colour channels interpolate in sRGB.
space_attributes_3d <- function(data, rows, weights) {
  out <- data[rows[, 1L], , drop = FALSE]
  continuous <- intersect(c("alpha", "linewidth", "size", "stroke", "angle", "hjust", "vjust", "nx", "ny", "nz", "u", "v"), names(data))
  for (name in continuous) if (is.numeric(data[[name]])) {
    out[[name]] <- rowSums(matrix(data[[name]][rows], nrow(rows)) * weights)
  }
  for (name in intersect(c("colour", "fill"), names(data))) {
    values <- matrix(data[[name]][rows], nrow(rows))
    varying <- apply(values, 1L, function(x) !anyNA(x) && length(unique(x)) > 1L)
    if (any(varying)) {
      rgba <- matrix(grDevices::col2rgb(as.vector(t(values[varying, , drop = FALSE])), alpha = TRUE), 4L)
      channels <- vapply(seq_len(4L), function(j) rowSums(matrix(rgba[j, ], ncol = ncol(rows), byrow = TRUE) * weights[varying, , drop = FALSE]), numeric(sum(varying)))
      channels <- matrix(channels, ncol = 4L)
      out[[name]][varying] <- grDevices::rgb(channels[, 1L], channels[, 2L], channels[, 3L], channels[, 4L], maxColorValue = 255)
    }
  }
  rownames(out) <- NULL
  out
}

# Primitive topology is established in the input chart before geodesic sampling.
# Every geometry using packet_from_primitives_3d shares this hook.
munch_3d <- function(data, primitives, kinds, counts, context) {
  sampler <- space_subdivision_3d(context)
  used <- unique(primitives)
  X <- space_canonical_3d(space_input_3d(data[used, , drop = FALSE], context), context)
  lookup <- match(primitives, used)
  offsets <- cumsum(c(1L, head(counts, -1L)))
  blocks <- list(); out_kinds <- list(); out_counts <- list(); starts <- list(); ends <- list()
  add <- function(d, xyz, kind, sizes, start = rep(TRUE, length(sizes)), end = rep(TRUE, length(sizes))) {
    d[c("x", "y", "z")] <- as.data.frame(xyz)
    blocks[[length(blocks) + 1L]] <<- d
    out_kinds[[length(out_kinds) + 1L]] <<- rep(kind, length(sizes))
    out_counts[[length(out_counts) + 1L]] <<- sizes
    starts[[length(starts) + 1L]] <<- start
    ends[[length(ends) + 1L]] <<- end
  }
  for (i in seq_along(counts)) {
    corners <- seq.int(offsets[i], length.out = counts[i])
    rows <- primitives[corners]; points <- X[lookup[corners], , drop = FALSE]
    kind <- kinds[i]
    if (kind == "polylines") {
      path <- polyline_model_3d(sampler, points, context$max_depth)
      endpoints <- cbind(rows[path$segment], rows[path$segment + 1L])
      d <- space_attributes_3d(data, endpoints, cbind(1 - path$s, path$s))
      fields <- intersect(c("colour", "alpha", "linewidth", "linetype"), names(d))
      if (any(vapply(d[fields], function(x) length(unique(x)) > 1L, logical(1)))) {
        index <- as.vector(rbind(seq_len(nrow(d) - 1L), seq_len(nrow(d) - 1L) + 1L))
        pieces <- seq_len(nrow(d) - 1L)
        add(d[index, , drop = FALSE], path$model[index, , drop = FALSE], kind, rep.int(2L, length(pieces)),
          start = pieces == 1L, end = pieces == length(pieces))
      } else add(d, path$model, kind, nrow(d))
    } else if (kind == "faces" && !sampler$projective) {
      triangles <- cbind(1L, 2:(nrow(points) - 1L), 3:nrow(points))
      mesh <- munch_triangles_3d(sampler, points[triangles[, 1L], , drop = FALSE],
        points[triangles[, 2L], , drop = FALSE], points[triangles[, 3L], , drop = FALSE], max_level = context$max_depth)
      parent <- triangles[mesh$point_parent, , drop = FALSE]
      d <- space_attributes_3d(data, matrix(rows[parent], ncol = 3L), mesh$weights)
      index <- as.vector(t(mesh$triangles))
      add(d[index, , drop = FALSE], mesh$model[index, , drop = FALSE], kind, rep.int(3L, nrow(mesh$triangles)))
    } else add(data[rows, , drop = FALSE], sampler$map(points), kind, length(rows))
  }
  out <- do.call(rbind, blocks); rownames(out) <- NULL
  list(data = out, primitives = seq_len(nrow(out)), kinds = unlist(out_kinds, use.names = FALSE),
    counts = unlist(out_counts, use.names = FALSE), start = unlist(starts), end = unlist(ends))
}

emit_model_scene_3d <- function(data,params,panel_params,ranges,layer_id=scene_furniture_layer_3d) {
  panel_params$ranges <- ranges
  spec <- space_spec_3d(SpaceEuclidean,"cartesian","cartesian",NULL,1e-3,9L)
  spec$identity <- FALSE
  panel_params$space_context <- bind_space_3d(spec,panel_params)
  panel_params$space_context$frame <- FrameBox
  emit_scene_3d(data,params,panel_params,layer_id)
}
