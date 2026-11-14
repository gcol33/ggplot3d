# Panel frames map normalised scale space, the unit cube of trained position
# ranges, to model space, the unit cube the camera sees. Every frame describes
# its furniture in scale space (panes, grid families, edges, axis candidates);
# the shared machinery below maps, subdivides and classifies it.

cube_panes_3d <- function() {
  lapply(c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax"), function(pane) {
    normal <- match(substr(pane, 1, 1), c("x", "y", "z"))
    side <- as.integer(substr(pane, 2, 4) == "max")
    other <- setdiff(seq_len(3L), normal)
    xyz <- matrix(0, 4L, 3L); xyz[, normal] <- side
    xyz[, other] <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1))
    outward <- numeric(3); outward[normal] <- 2 * side - 1
    families <- lapply(other, function(axis) {
      name <- c("x", "y", "z")[axis]
      coef <- numeric(3); coef[axis] <- 1
      list(scale = name, coef = coef, offset = 0, element = name)
    })
    list(name = pane, xyz = xyz, outward = outward, families = families, element = c("x", "y", "z")[normal])
  })
}

cube_frame_edges_3d <- function() {
  cube <- cube_edges_3d()
  lapply(seq_len(nrow(cube$edges)), function(i) {
    xyz <- cube$corners[cube$edges[i, c("a", "b")], , drop = FALSE]
    fixed <- which(xyz[1L, ] == xyz[2L, ])
    list(xyz = xyz, incident = paste0(c("x", "y", "z")[fixed], ifelse(xyz[1L, fixed] == 0, "min", "max")))
  })
}

cube_axis_candidates_3d <- function(axis) {
  cube <- cube_edges_3d()
  edges <- cube$edges[cube$edges[, "axis"] == axis, , drop = FALSE]
  lapply(seq_len(nrow(edges)), function(i) {
    list(start = cube$corners[edges[i, "a"], ], end = cube$corners[edges[i, "b"], ])
  })
}

Frame3D <- ggplot2::ggproto("Frame3D", NULL,
  name = "box",
  linear = TRUE,
  identity = FALSE,
  up = 3L,
  parts = character(),
  tolerance = 1e-3,
  bind = function(self, panel_params) self,
  axis_view = function(self, scale, limits, expand, axis) axis_view_3d(scale, limits, expand, axis),
  setup_aesthetics = function(self, data) data,
  guides = function(self, guides, panel_params) guides,
  map = function(self, xyz) xyz,
  panes = function(self) cube_panes_3d(),
  edges = function(self) cube_frame_edges_3d(),
  bounds = function(self) {
    if (self$linear) return(self$map(cube_edges_3d()$corners))
    unique(self$map(frame_surface_samples_3d(self$panes(), 16L)))
  },
  # Back panes are the rear one of each pair of vertical faces; the floor is
  # the low face of the vertical axis.
  back_panes = function(self, partitions) {
    vapply(c("x", "y", "z")[-self$up], function(axis) {
      candidates <- paste0(axis, c("min", "max"))
      candidates[which.min(partitions[candidates])]
    }, character(1))
  },
  rear_only = function(self, pane) FALSE,
  floor_panes = function(self) paste0(c("x", "y", "z")[self$up], "min"),
  back_candidates = function(self) paste0(rep(c("x", "y", "z")[-self$up], each = 2L), c("min", "max")),
  # The line of one scale axis through a scale-space point.
  origin_axis = function(self, axis, origin) {
    start <- end <- unname(origin)
    start[axis] <- 0; end[axis] <- 1
    list(start = start, end = end)
  },
  # The cross-section of the scale-space cube where one axis takes a value.
  section = function(self, axis, value) {
    other <- setdiff(seq_len(3L), axis)
    xyz <- matrix(value, 4L, 3L)
    xyz[, other] <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1))
    outward <- numeric(3); outward[axis] <- 1
    name <- c("x", "y", "z")[axis]
    list(name = paste0(name, "zero"), xyz = xyz, outward = outward, families = list(), element = name)
  },
  axis_candidates = function(self, guide) {
    axis <- match(guide$axis, c("x", "y", "z"))
    candidates <- cube_axis_candidates_3d(axis)
    attr(candidates, "auto") <- if (axis != self$up) "lower" else "outer"
    candidates
  }
)

FrameBox <- ggplot2::ggproto("FrameBox", Frame3D,
  identity = TRUE,
  transform = list(),
  bind = function(self, panel_params) {
    if (!length(self$transform)) return(self)
    ranges <- panel_params$ranges
    maps <- lapply(c("x", "y", "z"), function(axis) {
      trans <- self$transform[[axis]]
      if (is.null(trans)) return(NULL)
      range <- ranges[[axis]]
      ends <- trans$transform(range)
      if (any(!is.finite(ends)) || ends[1L] == ends[2L]) stop("The ", axis, " coordinate transformation is undefined over the panel range.", call. = FALSE)
      function(u) {
        value <- trans$transform(range[1L] + u * diff(range))
        (value - ends[1L]) / (ends[2L] - ends[1L])
      }
    })
    ggplot2::ggproto(NULL, self, linear = FALSE, identity = FALSE, axis_maps = maps)
  },
  axis_maps = list(),
  axis_view = function(self, scale, limits, expand, axis) {
    trans <- self$transform[[axis]]
    if (is.null(trans) || scale$is_discrete()) return(axis_view_3d(scale, limits, expand, axis))
    scale_trans <- scale$get_transformation()
    range <- if (is.null(limits)) scale$dimension() else scale_trans$transform(limits)
    range[!is.finite(range)] <- scale$dimension()[!is.finite(range)]
    ends <- trans$transform(range)
    if (any(!is.finite(ends))) stop("The ", axis, " coordinate transformation is undefined over the trained range.", call. = FALSE)
    if (isTRUE(expand)) {
      factors <- scale$expand
      if (is.null(factors) || inherits(factors, "waiver")) factors <- c(0.05, 0, 0.05, 0)
      factors <- rep_len(factors, 4L)
      width <- diff(ends)
      ends <- ends + c(-(factors[1L] * width + factors[2L]), factors[3L] * width + factors[4L])
    }
    axis_view_3d(scale, scale_trans$inverse(trans$inverse(ends)), FALSE, axis)
  },
  map = function(self, xyz) {
    for (axis in seq_along(self$axis_maps)) {
      f <- self$axis_maps[[axis]]
      if (!is.null(f)) xyz[, axis] <- f(xyz[, axis])
    }
    xyz
  }
)

frame_box_3d <- function(transform = NULL) {
  if (is.null(transform) || !length(transform)) return(FrameBox)
  if (!is.list(transform) || is.null(names(transform)) || !all(names(transform) %in% c("x", "y", "z")) || anyDuplicated(names(transform))) {
    stop("transform must be a named list with entries x, y or z.", call. = FALSE)
  }
  transform <- lapply(transform, function(t) if (is.character(t)) scales::as.transform(t) else t)
  if (!all(vapply(transform, inherits, logical(1), "transform"))) stop("Each coordinate transformation must name or be a scales transformation.", call. = FALSE)
  ggplot2::ggproto(NULL, FrameBox, transform = transform)
}

frame_of_3d <- function(panel_params) panel_params$frame %or3d% FrameBox

check_frame_panes_3d <- function(frame, panes) {
  valid <- c("back", "floor", "front", vapply(frame$panes(), `[[`, character(1), "name"))
  if (!all(panes %in% valid)) stop("Unknown pane for this frame: ", paste(setdiff(panes, valid), collapse = ", "), ". Valid panes: ", paste(valid, collapse = ", "), ".", call. = FALSE)
  invisible(panes)
}

# Binds a frame to a panel: per-panel ranges, and the camera's vertical axis.
bind_frame_3d <- function(frame, panel_params) {
  bound <- frame$bind(panel_params)
  up <- as.integer(panel_params$camera$up %or3d% 3L)
  if (up != bound$up) return(ggplot2::ggproto(NULL, bound, up = up))
  bound
}

# Emitted packets hold scale-space positions; the frame takes them to model space.
frame_packet_3d <- function(frame, packet, clip = TRUE) {
  if (isTRUE(frame$identity)) return(packet)
  packet <- munch_packet_3d(frame, packet, clip)
  attr(packet, "parent") <- NULL
  attr(packet, "scale") <- NULL
  packet
}

frame_surface_samples_3d <- function(panes, n) {
  grid <- as.matrix(expand.grid(s = seq(0, 1, length.out = n + 1L), t = seq(0, 1, length.out = n + 1L)))
  do.call(rbind, lapply(panes, function(pane) {
    xyz <- pane$xyz
    if (nrow(xyz) == 3L) {
      inside <- grid[rowSums(grid) <= 1 + 1e-12, , drop = FALSE]
      return(xyz[rep(1L, nrow(inside)), , drop = FALSE] + inside[, 1L] %o% (xyz[2L, ] - xyz[1L, ]) + inside[, 2L] %o% (xyz[3L, ] - xyz[1L, ]))
    }
    a <- xyz[1L, ]; b <- xyz[2L, ]; d <- xyz[4L, ]
    xyz[rep(1L, nrow(grid)), , drop = FALSE] + grid[, 1L] %o% (b - a) + grid[, 2L] %o% (d - a)
  }))
}

# Subdivision -----------------------------------------------------------------

# Points at parameters s along canonical segments lo -> hi. The same endpoints
# and parameters give identical bits from every primitive sharing the segment.
segment_points_3d <- function(lo, hi, s) (1 - s) * lo + s * hi

canonical_segments_3d <- function(p, q) {
  swap <- rep(FALSE, nrow(p)); equal <- rep(TRUE, nrow(p))
  for (j in seq_len(ncol(p))) {
    swap <- swap | (equal & p[, j] > q[, j])
    equal <- equal & p[, j] == q[, j]
  }
  lo <- p; hi <- q
  lo[swap, ] <- q[swap, ]; hi[swap, ] <- p[swap, ]
  list(lo = lo, hi = hi, swap = swap)
}

# Power-of-two subdivision level per segment so that every mapped sub-segment
# stays within tolerance of its chord at the sub-segment midpoint.
segment_levels_3d <- function(frame, lo, hi, tolerance = frame$tolerance, max_level = 9L) {
  n <- nrow(lo)
  level <- integer(n)
  if (isTRUE(frame$projective)) return(level)
  active <- seq_len(n)
  for (current in 0:max_level) {
    if (!length(active)) break
    m <- 2^current
    edge <- rep(active, each = m)
    j <- rep(seq_len(m) - 1, length(active))
    sample <- function(s) subdivision_metric_3d(frame, frame$map(subdivision_points_3d(frame,
      lo[edge, , drop = FALSE], hi[edge, , drop = FALSE], s)))
    a <- sample(j / m); b <- sample((j + 1) / m)
    probes <- if (isTRUE(frame$strict)) c(0.25, 0.5, 0.75) else 0.5
    deviation <- Reduce(pmax, lapply(probes, function(s) chord_distance_3d(sample((j + s) / m), a, b, segment = isTRUE(frame$strict))))
    worst <- vapply(split(deviation, factor(edge, levels = active)), max, numeric(1))
    level[active] <- current
    active <- active[worst > tolerance]
  }
  if (length(active) && isTRUE(frame$strict)) stop("Geodesic subdivision exceeded max_depth before reaching munch_tolerance.", call. = FALSE)
  level
}

subdivision_points_3d <- function(frame, lo, hi, s) {
  if (is.function(frame$interpolate)) frame$interpolate(lo, hi, s) else segment_points_3d(lo, hi, s)
}

subdivision_metric_3d <- function(frame, xyz) {
  if (is.function(frame$metric)) frame$metric(xyz) else model_points_3d(xyz)
}

# Euclidean rows of mapped model points, which are three Euclidean or four
# homogeneous columns.
model_points_3d <- function(xyz) if (ncol(xyz) == 4L) homogeneous_xyz_3d(xyz) else xyz

# A frame applied to homogeneous rows (x, y, z, w). An affine frame maps a
# finite point p/w to map(p/w) w, keeping w, and a direction d (w = 0) to
# map(d) - map(0); other frames need finite points.
frame_map_homogeneous_3d <- function(frame, xyzw) {
  if (ncol(xyzw) == 3L) return(frame$map(xyzw))
  if (isTRUE(frame$identity)) return(xyzw)
  if (!isTRUE(frame$linear)) return(cbind(frame$map(homogeneous_xyz_3d(xyzw)), 1))
  out <- xyzw
  finite <- xyzw[, 4L] > 0
  if (any(finite)) out[finite, 1:3] <- frame$map(xyzw[finite, 1:3, drop = FALSE] / xyzw[finite, 4L]) * xyzw[finite, 4L]
  if (any(!finite)) out[!finite, 1:3] <- frame$map(xyzw[!finite, 1:3, drop = FALSE]) -
    frame$map(matrix(0, sum(!finite), 3L))
  out
}

triangle_points_3d <- function(frame, p0, p1, p2, a, b) {
  if (!is.function(frame$interpolate)) return((1 - a - b) * p0 + a * p1 + b * p2)
  total <- a + b
  edge <- subdivision_points_3d(frame, p1, p2, ifelse(total > 0, b / total, 0))
  subdivision_points_3d(frame, p0, edge, total)
}

# Distance of mapped midpoints from the line through the mapped endpoints: a
# straight image with uneven parametrisation needs no subdivision.
chord_distance_3d <- function(m, a, b, segment = FALSE) {
  d <- b - a; length <- sqrt(rowSums(d^2))
  v <- m - a
  along <- rowSums(v * d) / ifelse(length > 0, length^2, 1)
  if (segment) along <- pmax(0, pmin(1, along))
  out <- sqrt(rowSums((v - along * d)^2))
  out[!(length > 0)] <- sqrt(rowSums(v^2))[!(length > 0)]
  out[!is.finite(out)] <- Inf
  out
}

# Model-space points of a canonical segment at parameters s (from lo), each
# interpolated between the mapped points of the segment's own subdivision so
# that neighbouring primitives agree exactly along a shared segment.
segment_model_3d <- function(frame, lo, hi, level, s) {
  n <- 2^level
  position <- s * n
  j0 <- pmin(floor(position), n - 1)
  f <- position - j0
  a <- frame$map(subdivision_points_3d(frame, lo, hi, j0 / n))
  exact <- f == 0
  out <- a
  if (any(!exact)) {
    k <- which(!exact)
    b <- frame$map(subdivision_points_3d(frame, lo[k, , drop = FALSE], hi[k, , drop = FALSE], (j0[k] + 1) / n[k]))
    out[k, ] <- (1 - f[k]) * a[k, , drop = FALSE] + f[k] * b
  }
  out
}

polyline_model_3d <- function(frame, xyz, max_level = 9L) {
  if (nrow(xyz) < 2L) return(list(model = frame$map(xyz), scale = xyz))
  p <- xyz[-nrow(xyz), , drop = FALSE]; q <- xyz[-1L, , drop = FALSE]
  seg <- canonical_segments_3d(p, q)
  level <- segment_levels_3d(frame, seg$lo, seg$hi, max_level = max_level)
  pieces <- lapply(seq_len(nrow(p)), function(i) {
    m <- 2^level[i]
    s <- (seq_len(m + 1L) - 1) / m
    if (seg$swap[i]) s <- 1 - s
    lo <- seg$lo[rep(i, length(s)), , drop = FALSE]; hi <- seg$hi[rep(i, length(s)), , drop = FALSE]
    model <- segment_model_3d(frame, lo, hi, rep(level[i], length(s)), s)
    scale <- subdivision_points_3d(frame, lo, hi, s)
    keep <- if (i > 1L) -1L else seq_along(s)
    list(model = model[keep, , drop = FALSE], scale = scale[keep, , drop = FALSE],
      segment = rep.int(i, nrow(model[keep, , drop = FALSE])), s = (if (seg$swap[i]) 1 - s else s)[keep])
  })
  list(model = do.call(rbind, lapply(pieces, `[[`, "model")), scale = do.call(rbind, lapply(pieces, `[[`, "scale")),
    segment = unlist(lapply(pieces, `[[`, "segment")), s = unlist(lapply(pieces, `[[`, "s")))
}

# Triangles in scale space become a lattice of 4^k triangles per input, with
# edge points on the shared canonical segment subdivision.
triangle_lattice_3d <- function(k) {
  ij <- do.call(rbind, lapply(0:k, function(i) cbind(i = i, j = 0:(k - i))))
  key <- function(i, j) match(paste(i, j), paste(ij[, 1L], ij[, 2L]))
  up <- ij[rowSums(ij) < k, , drop = FALSE]
  down <- ij[rowSums(ij) < k - 1L, , drop = FALSE]
  triangles <- rbind(cbind(key(up[, 1L], up[, 2L]), key(up[, 1L] + 1L, up[, 2L]), key(up[, 1L], up[, 2L] + 1L)),
    cbind(key(down[, 1L] + 1L, down[, 2L]), key(down[, 1L] + 1L, down[, 2L] + 1L), key(down[, 1L], down[, 2L] + 1L)))
  list(ij = ij, triangles = triangles)
}

munch_triangles_3d <- function(frame, p0, p1, p2, boundary = matrix(TRUE, nrow(p0), 3L), max_level = 6L) {
  n <- nrow(p0)
  if (!n) return(list(model = matrix(numeric(), 0L, 3L), triangles = matrix(integer(), 0L, 3L), parent = integer(), point_parent = integer(), scale = matrix(numeric(), 0L, 3L)))
  edges <- list(canonical_segments_3d(p0, p1), canonical_segments_3d(p1, p2), canonical_segments_3d(p2, p0))
  levels <- vapply(edges, function(e) segment_levels_3d(frame, e$lo, e$hi,
    max_level = if (isTRUE(frame$strict)) max_level else 9L), integer(n))
  if (n == 1L) levels <- matrix(levels, 1L)
  levels[!boundary] <- 0L
  m0 <- model_points_3d(frame$map(p0)); m1 <- model_points_3d(frame$map(p1)); m2 <- model_points_3d(frame$map(p2))
  normal <- cross_rows_3d(m1 - m0, m2 - m0)
  area <- sqrt(rowSums(normal^2))
  offset <- model_points_3d(frame$map(if (is.function(frame$interpolate)) triangle_points_3d(frame, p0, p1, p2, 1/3, 1/3) else (p0 + p1 + p2) / 3)) - m0
  deviation <- ifelse(area > 0, abs(rowSums(offset * normal)) / area, sqrt(rowSums(offset^2)))
  interior <- ifelse(is.finite(deviation) & deviation > frame$tolerance, ceiling(log2(sqrt(deviation / frame$tolerance))), 0)
  levels <- pmin(levels, max_level)
  level <- pmin(max_level, pmax(apply(levels, 1L, max), as.integer(interior)))
  if (isTRUE(frame$strict)) {
    level <- vapply(seq_len(n), function(i) triangle_level_3d(frame,
      p0[i, , drop = FALSE], p1[i, , drop = FALSE], p2[i, , drop = FALSE], level[i], max_level), integer(1))
  }
  model <- list(); scale <- list(); triangles <- list(); parent <- list(); point_parent <- list(); weights <- list(); offset <- 0L
  for (lv in sort(unique(level))) {
    rows <- which(level == lv)
    k <- 2L^lv
    lattice <- triangle_lattice_3d(k)
    m <- nrow(lattice$ij)
    owner <- rep(rows, each = m)
    i <- rep(lattice$ij[, 1L], length(rows)); j <- rep(lattice$ij[, 2L], length(rows))
    a <- i / k; b <- j / k
    xyz <- triangle_points_3d(frame, p0[owner, , drop = FALSE], p1[owner, , drop = FALSE], p2[owner, , drop = FALSE], a, b)
    out <- frame$map(xyz)
    on_edge <- list(j == 0L, i + j == k, i == 0L)
    params <- list(a, b, 1 - b)
    for (e in 1:3) {
      idx <- which(on_edge[[e]])
      s <- params[[e]][idx]
      r <- owner[idx]
      s[edges[[e]]$swap[r]] <- 1 - s[edges[[e]]$swap[r]]
      out[idx, ] <- segment_model_3d(frame, edges[[e]]$lo[r, , drop = FALSE], edges[[e]]$hi[r, , drop = FALSE], levels[cbind(r, e)], s)
    }
    block <- (seq_along(rows) - 1L) * m + offset
    model[[length(model) + 1L]] <- out
    scale[[length(scale) + 1L]] <- xyz
    triangles[[length(triangles) + 1L]] <- lattice$triangles[rep(seq_len(nrow(lattice$triangles)), length(rows)), , drop = FALSE] +
      rep(block, each = nrow(lattice$triangles))
    parent[[length(parent) + 1L]] <- rep(rows, each = nrow(lattice$triangles))
    point_parent[[length(point_parent) + 1L]] <- owner
    weights[[length(weights) + 1L]] <- cbind(1 - a - b, a, b)
    offset <- offset + m * length(rows)
  }
  list(model = do.call(rbind, model), triangles = do.call(rbind, triangles),
    parent = unlist(parent, use.names = FALSE), point_parent = unlist(point_parent, use.names = FALSE),
    scale = do.call(rbind, scale), weights = do.call(rbind, weights))
}

# Samples each subtriangle at its barycentre in the drawing metric.
triangle_level_3d <- function(frame, p0, p1, p2, level, max_level) {
  for (current in level:max_level) {
    lattice <- triangle_lattice_3d(2^current)
    ab <- lattice$ij / 2^current
    sample <- function(weights) frame$map(triangle_points_3d(frame,
      p0[rep(1L, nrow(weights)), , drop = FALSE], p1[rep(1L, nrow(weights)), , drop = FALSE],
      p2[rep(1L, nrow(weights)), , drop = FALSE], weights[, 1L], weights[, 2L]))
    vertices <- model_points_3d(sample(ab))
    t <- lattice$triangles
    centroid <- (ab[t[, 1L], , drop = FALSE] + ab[t[, 2L], , drop = FALSE] + ab[t[, 3L], , drop = FALSE]) / 3
    chord <- (vertices[t[, 1L], , drop = FALSE] + vertices[t[, 2L], , drop = FALSE] + vertices[t[, 3L], , drop = FALSE]) / 3
    error <- sqrt(rowSums((subdivision_metric_3d(frame, sample(centroid)) - subdivision_metric_3d(frame, chord))^2))
    if (all(is.finite(error)) && max(error) <= frame$tolerance) return(as.integer(current))
  }
  stop("Geodesic face subdivision exceeded max_depth before reaching munch_tolerance.", call. = FALSE)
}

# Packet munching ---------------------------------------------------------------

# Normals (rows) under a linear map with columns jacobian[[1]], [[2]], [[3]]
# (each a matrix of rows, one map per normal): the inverse transpose, whose
# columns are the cross products of pairs of map columns over the
# determinant, renormalised.
cotransform_normals_3d <- function(normal, jacobian) {
  a <- jacobian[[1L]]; b <- jacobian[[2L]]; c <- jacobian[[3L]]
  bc <- cross_rows_3d(b, c); ca <- cross_rows_3d(c, a); ab <- cross_rows_3d(a, b)
  out <- normal[, 1L] * bc + normal[, 2L] * ca + normal[, 3L] * ab
  out <- out * sign(rowSums(a * bc))
  out / sqrt(rowSums(out^2))
}

# Corner columns with their finite normals mapped by `cotransform(normal,
# rows)`, which receives the finite rows and their row numbers.
map_corner_normals_3d <- function(corners, cotransform) {
  names <- c("normal_x", "normal_y", "normal_z")
  if (!all(names %in% names(corners))) return(corners)
  normal <- cbind(corners$normal_x, corners$normal_y, corners$normal_z)
  ok <- which(rowSums(is.finite(normal)) == 3L)
  if (!length(ok)) return(corners)
  mapped <- cotransform(normal[ok, , drop = FALSE], ok)
  for (k in 1:3) corners[[names[k]]][ok] <- mapped[, k]
  corners
}

# Maps a scale-space packet through a frame. Linear frames map vertices; other
# frames clip to the panel box in scale space, then subdivide faces and
# strokes so curved images stay within tolerance. A linear (affine) frame maps
# a direction (w = 0) d to map(d) - map(0); other frames need finite vertices,
# which clipping to the panel box leaves.
munch_packet_3d <- function(frame, packet, clip = TRUE, coordinates = NULL) {
  validate_scene_packet(packet)
  if (frame$linear) {
    out <- if (is.null(coordinates)) frame_map_homogeneous_3d(frame, scene_vertex_matrix_3d(packet$vertices)) else frame$map(coordinates)
    packet$vertices <- scene_vertices_3d(out)
    packet$corners <- map_corner_normals_3d(packet$corners, function(normal, rows) {
      origin <- frame$map(matrix(0, 1L, 3L))
      jacobian <- lapply(1:3, function(k) {
        column <- frame$map(diag(3)[k, , drop = FALSE]) - origin
        matrix(column, nrow(normal), 3L, byrow = TRUE)
      })
      cotransform_normals_3d(normal, jacobian)
    })
    validate_scene_packet(packet)
    return(packet)
  }
  clipped <- NULL
  if (clip) {
    packet <- clip_packet_planes_3d(packet, unit_box_planes_3d())
    clipped <- attr(packet, "parent")
  }
  scale_xyz <- coordinates %or3d% scene_vertices_xyz_3d(packet$vertices)
  corner_xyz <- function(rows) scale_xyz[packet$corners$vertex[rows], , drop = FALSE]
  out_xyz <- list(); out_rows <- list(); out_scale <- list(); n_points <- 0L
  emit <- function(model, source_row, scale) {
    out_xyz[[length(out_xyz) + 1L]] <<- model
    out_scale[[length(out_scale) + 1L]] <<- scale
    out_rows[[length(out_rows) + 1L]] <<- source_row
    first <- n_points + 1L
    n_points <<- n_points + nrow(model)
    seq.int(first, length.out = nrow(model))
  }
  # Faces: fan-triangulate, subdivide.
  faces <- packet$faces
  tri <- list(p0 = list(), p1 = list(), p2 = list(), parent = integer(), boundary = list(), corners = list())
  for (f in seq_along(faces$offset)) {
    xyz <- corner_xyz(seq.int(faces$offset[f], length.out = faces$count[f]))
    for (i in 2:(nrow(xyz) - 1L)) {
      tri$p0[[length(tri$p0) + 1L]] <- xyz[1L, ]; tri$p1[[length(tri$p1) + 1L]] <- xyz[i, ]; tri$p2[[length(tri$p2) + 1L]] <- xyz[i + 1L, ]
      tri$corners[[length(tri$corners) + 1L]] <- faces$offset[f] + c(0L, i - 1L, i)
      tri$parent <- c(tri$parent, f)
      tri$boundary[[length(tri$boundary) + 1L]] <- if (isTRUE(frame$strict)) rep(TRUE, 3L) else c(i == 2L, TRUE, i == nrow(xyz) - 1L)
    }
  }
  face_rows <- list(corners = integer(), parent = integer())
  attributes <- setdiff(names(packet$corners), c("vertex", "source_row"))
  out_attributes <- list()
  if (length(tri$parent)) {
    munched <- munch_triangles_3d(frame, do.call(rbind, tri$p0), do.call(rbind, tri$p1), do.call(rbind, tri$p2),
      do.call(rbind, tri$boundary), max_level = frame$max_level %or3d% 6L)
    parents <- tri$parent[munched$parent]
    a <- munched$model[munched$triangles[, 1L], , drop = FALSE]
    b <- munched$model[munched$triangles[, 2L], , drop = FALSE]
    c <- munched$model[munched$triangles[, 3L], , drop = FALSE]
    a <- model_points_3d(a); b <- model_points_3d(b); c <- model_points_3d(c)
    area <- sqrt(rowSums(cross_rows_3d(b - a, c - a)^2))
    keep <- is.finite(area) & area > 1e-12
    ids <- emit(munched$model, faces$source_row[tri$parent[munched$point_parent]], munched$scale)
    if (length(attributes)) {
      # Corner attributes are barycentric in the scale-space triangle; normals
      # then follow the frame's local linear map.
      from <- do.call(rbind, tri$corners)[munched$point_parent, , drop = FALSE]
      values <- lapply(packet$corners[attributes], function(a)
        rowSums(munched$weights * matrix(a[from], ncol = 3L)))
      point <- munched$scale
      corner_set <- map_corner_normals_3d(values, function(normal, rows) {
        h <- 1e-6
        at <- point[rows, , drop = FALSE]
        jacobian <- lapply(1:3, function(k) {
          step <- matrix(0, nrow(at), 3L); step[, k] <- h
          (frame$map(at + step) - frame$map(at - step)) / (2 * h)
        })
        cotransform_normals_3d(normal, jacobian)
      })
      out_attributes[[length(out_attributes) + 1L]] <- list(ids = ids, values = corner_set)
    }
    face_rows$corners <- ids[as.vector(t(munched$triangles[keep, , drop = FALSE]))]
    face_rows$parent <- parents[keep]
  }
  # Polylines: subdivide each segment canonically.
  line <- packet$polylines
  line_rows <- list(corners = integer(), count = integer(), parent = integer())
  for (l in seq_along(line$offset)) {
    xyz <- corner_xyz(seq.int(line$offset[l], length.out = line$count[l]))
    path <- polyline_model_3d(frame, xyz, max_level = frame$max_level %or3d% 9L)
    ids <- emit(path$model, rep.int(line$source_row[l], nrow(path$model)), path$scale)
    line_rows$corners <- c(line_rows$corners, ids)
    line_rows$count <- c(line_rows$count, length(ids))
    line_rows$parent <- c(line_rows$parent, l)
  }
  # Markers and text: map anchors.
  point_rows <- lapply(c("markers", "text"), function(kind) {
    tab <- packet[[kind]]
    list(xyz = corner_xyz(tab$offset), parent = seq_along(tab$offset))
  }); names(point_rows) <- c("markers", "text")
  for (kind in c("markers", "text")) {
    rows <- point_rows[[kind]]
    point_rows[[kind]]$corners <- if (length(rows$parent)) emit(frame$map(rows$xyz), packet[[kind]]$source_row[rows$parent], rows$xyz) else integer()
  }
  model <- if (length(out_xyz)) do.call(rbind, out_xyz) else matrix(numeric(), 0L, 3L)
  if (ncol(model) == 3L) model <- cbind(model, 1)
  scale_points <- if (length(out_scale)) do.call(rbind, out_scale) else matrix(numeric(), 0L, 3L)
  source_rows <- as.integer(unlist(out_rows, use.names = FALSE))
  key <- paste(sprintf("%.17g", model[, 1L]), sprintf("%.17g", model[, 2L]), sprintf("%.17g", model[, 3L]), sprintf("%.17g", model[, 4L]))
  unique_index <- match(key, key)
  used <- sort(unique(unique_index))
  vertex_of <- match(unique_index, used)
  build <- function(tab, parent, counts, corners) {
    out <- lapply(tab, function(column) column[parent])
    out$count <- as.integer(counts)
    out$offset <- if (length(counts)) as.integer(cumsum(c(0L, head(counts, -1L))) + 1L) else integer()
    list(table = out, corners = corners)
  }
  pieces <- list(
    faces = build(faces, face_rows$parent, rep.int(3L, length(face_rows$parent)), face_rows$corners),
    polylines = build(line, line_rows$parent, line_rows$count, line_rows$corners),
    markers = build(packet$markers, point_rows$markers$parent, rep.int(1L, length(point_rows$markers$parent)), point_rows$markers$corners),
    text = build(packet$text, point_rows$text$parent, rep.int(1L, length(point_rows$text$parent)), point_rows$text$corners))
  corner_ids <- integer(); shift <- 0L; next_id <- 0L
  for (kind in names(pieces)) {
    tab <- pieces[[kind]]$table
    tab$offset <- as.integer(tab$offset + shift)
    shift <- shift + length(pieces[[kind]]$corners)
    corner_ids <- c(corner_ids, pieces[[kind]]$corners)
    tab$primitive_id <- as.integer(next_id + seq_along(tab$offset))
    next_id <- next_id + length(tab$offset)
    packet[[kind]] <- tab
  }
  packet$vertices <- scene_vertices_3d(model[used, , drop = FALSE])
  packet$corners <- list(vertex = as.integer(vertex_of[corner_ids]), source_row = as.integer(source_rows[corner_ids]))
  for (name in attributes) {
    value <- rep(NA_real_, n_points)
    for (part in out_attributes) value[part$ids] <- part$values[[name]]
    packet$corners[[name]] <- value[corner_ids]
  }
  if (all(c("normal_x", "normal_y", "normal_z") %in% names(packet$styles))) {
    packet$styles$normal_x <- NA_real_; packet$styles$normal_y <- NA_real_; packet$styles$normal_z <- NA_real_
  }
  validate_scene_packet(packet)
  parent <- list(faces = face_rows$parent, polylines = line_rows$parent,
    markers = point_rows$markers$parent, text = point_rows$text$parent)
  if (!is.null(clipped)) for (kind in names(parent)) parent[[kind]] <- clipped[[kind]][parent[[kind]]]
  attr(packet, "parent") <- parent
  attr(packet, "scale") <- scale_points[used, , drop = FALSE]
  packet
}

cross_rows_3d <- function(u, v) {
  cbind(u[, 2L] * v[, 3L] - u[, 3L] * v[, 2L], u[, 3L] * v[, 1L] - u[, 1L] * v[, 3L], u[, 1L] * v[, 2L] - u[, 2L] * v[, 1L])
}

# Local linear map from normalized panel coordinates into model coordinates.
frame_jacobian_3d <- function(frame, points) {
  jacobian <- array(diag(3), c(3L, 3L, nrow(points)))
  if (isTRUE(frame$identity)) return(jacobian)
  h <- 1e-6
  for (j in 1:3) {
    step <- matrix(0, nrow(points), 3L); step[, j] <- h
    jacobian[, j, ] <- t((frame$map(points + step) - frame$map(points - step)) / (2 * h))
  }
  jacobian
}

# Facing ------------------------------------------------------------------------

# Unit outward normals of a pane in aspect-scaled model space at scale points.
pane_normals_3d <- function(frame, pane, points, camera) {
  scale <- camera$aspect / max(camera$aspect)
  xyz <- pane$xyz
  h <- 1e-5
  difference <- function(direction) {
    step <- h * matrix(direction, nrow(points), 3L, byrow = TRUE)
    sweep(frame$map(points + step) - frame$map(points - step), 2L, scale, "*")
  }
  n <- cross_rows_3d(difference(xyz[2L, ] - xyz[1L, ]), difference(xyz[nrow(xyz), ] - xyz[1L, ]))
  outward <- difference(pane$outward)
  centre <- sweep(sweep(frame$map(points), 2L, scale, "*"), 2L, drop(frame$map(matrix(0.5, 1L, 3L))) * scale)
  weak <- rowSums(outward^2) <= (h * 1e-6)^2
  outward[weak, ] <- centre[weak, , drop = FALSE]
  flip <- rowSums(n * outward) < 0
  n[flip, ] <- -n[flip, ]
  n / sqrt(rowSums(n^2))
}

facing_3d <- function(normals, model_points, camera, basis) {
  facing <- rowSums(normals * toward_camera_3d(basis, camera, model_points))
  tolerance <- 64 * .Machine$double.eps * pmax(1, abs(facing))
  ifelse(facing > tolerance, 2L, 0L)
}
