cross2_3d <- function(a, b) a[1L] * b[2L] - a[2L] * b[1L]

ring_area_3d <- function(xy, ring) {
  next_ring <- c(ring[-1L], ring[1L])
  sum(xy[ring, 1L] * xy[next_ring, 2L] - xy[ring, 2L] * xy[next_ring, 1L]) / 2
}

inside_ring_3d <- function(point, xy, ring) {
  a <- xy[ring, , drop = FALSE]
  b <- a[c(seq.int(2L, nrow(a)), 1L), , drop = FALSE]
  crossing <- (a[, 2] > point[2]) != (b[, 2] > point[2])
  sum(point[1] < (b[crossing, 1] - a[crossing, 1]) * (point[2] - a[crossing, 2]) /
        (b[crossing, 2] - a[crossing, 2]) + a[crossing, 1]) %% 2L == 1L
}

segments_cross_3d <- function(a, b, c, d, tolerance) {
  ab <- c(cross2_3d(b - a, c - a), cross2_3d(b - a, d - a))
  cd <- c(cross2_3d(d - c, a - c), cross2_3d(d - c, b - c))
  if (prod(sign(ab)) < 0 && prod(sign(cd)) < 0 && all(abs(c(ab, cd)) > tolerance)) return(TRUE)
  on_segment <- function(p, u, v) all(p >= pmin(u, v) - tolerance & p <= pmax(u, v) + tolerance)
  (abs(ab[1]) <= tolerance && on_segment(c, a, b)) ||
    (abs(ab[2]) <= tolerance && on_segment(d, a, b)) ||
    (abs(cd[1]) <= tolerance && on_segment(a, c, d)) ||
    (abs(cd[2]) <= tolerance && on_segment(b, c, d))
}

# A visible bridge joins a clockwise hole to its containing counterclockwise ring.
bridge_ring_3d <- function(outer, hole, xy, rings, tolerance) {
  h <- which.max(xy[hole, 1L]); vertex <- hole[h]
  candidates <- order(rowSums((sweep(xy[outer, , drop = FALSE], 2L, xy[vertex, ]))^2))
  for (k in candidates) {
    a <- xy[vertex, ]; b <- xy[outer[k], ]; midpoint <- (a + b) / 2
    if (!inside_ring_3d(midpoint, xy, outer) || inside_ring_3d(midpoint, xy, hole)) next
    blocked <- FALSE
    for (ring in rings) for (j in seq_along(ring)) {
      pair <- ring[c(j, if (j == length(ring)) 1L else j + 1L)]
      if (any(pair %in% c(vertex, outer[k]))) next
      if (segments_cross_3d(a, b, xy[pair[1L], ], xy[pair[2L], ], tolerance)) blocked <- TRUE
    }
    if (blocked) next
    cycle <- hole[c(seq.int(h, length(hole)), if (h > 1L) seq_len(h - 1L))]
    return(c(outer[seq_len(k)], cycle, vertex, outer[k], if (k < length(outer)) outer[seq.int(k + 1L, length(outer))]))
  }
  stop("Polygon hole has no visible bridge to its containing ring.", call. = FALSE)
}

# Ear clipping preserves original vertex references while removing convex ears.
ear_triangles_3d <- function(xy, ring, tolerance) {
  triangles <- list()
  while (length(ring) > 3L) {
    clipped <- FALSE
    for (i in seq_along(ring)) {
      locations <- c(if (i == 1L) length(ring) else i - 1L, i, if (i == length(ring)) 1L else i + 1L)
      ids <- ring[locations]; tri <- xy[ids, , drop = FALSE]
      area <- cross2_3d(tri[2, ] - tri[1, ], tri[3, ] - tri[1, ])
      if (area <= tolerance) next
      other <- setdiff(unique(ring), ids)
      contains <- vapply(other, function(j) {
        p <- xy[j, ]
        if (any(rowSums((sweep(tri, 2, p))^2) <= tolerance^2)) return(FALSE)
        all(vapply(1:3, function(k) cross2_3d(tri[if (k == 3L) 1L else k + 1L, ] - tri[k, ], p - tri[k, ]), numeric(1)) >= -tolerance)
      }, logical(1))
      if (any(contains)) next
      triangles[[length(triangles) + 1L]] <- ids
      ring <- ring[-i]; clipped <- TRUE; break
    }
    if (!clipped) {
      collinear <- which(vapply(seq_along(ring), function(i) {
        prev <- if (i == 1L) length(ring) else i - 1L
        next_i <- if (i == length(ring)) 1L else i + 1L
        abs(cross2_3d(xy[ring[i], ] - xy[ring[prev], ], xy[ring[next_i], ] - xy[ring[i], ])) <= tolerance
      }, logical(1)))
      if (!length(collinear)) stop("Polygon triangulation requires simple nonintersecting rings.", call. = FALSE)
      ring <- ring[-collinear[1L]]
    }
  }
  if (length(ring) == 3L && abs(ring_area_3d(xy, ring)) > tolerance) triangles[[length(triangles) + 1L]] <- ring
  triangles
}

triangulate_polygon_3d <- function(data, rings) {
  xyz <- as.matrix(data[c("x", "y", "z")])
  centred <- sweep(xyz, 2L, colMeans(xyz))
  decomposition <- svd(centred, nu = 0L)
  tolerance <- 256 * .Machine$double.eps * max(1, max(abs(centred)))
  if (length(decomposition$d) < 2L || decomposition$d[2L] <= tolerance) stop("Polygon vertices are collinear.", call. = FALSE)
  if (length(decomposition$d) >= 3L && decomposition$d[3L] > tolerance * sqrt(nrow(data))) stop("Polygon rings must be planar after scale transformation.", call. = FALSE)
  xy <- centred %*% decomposition$v[, 1:2, drop = FALSE]
  rings <- lapply(rings, function(r) {
    if (length(r) > 3L && all(xyz[r[1L], ] == xyz[r[length(r)], ])) r <- r[-length(r)]
    if (length(r) < 3L || anyDuplicated(as.data.frame(xyz[r, , drop = FALSE]))) stop("Polygon rings require at least three distinct vertices.", call. = FALSE)
    r
  })
  for (i in seq_along(rings)) for (j in seq.int(i, length(rings))) {
    a <- rings[[i]]; b <- rings[[j]]
    for (u in seq_along(a)) for (v in seq_along(b)) {
      e <- a[c(u, if (u == length(a)) 1L else u + 1L)]
      f <- b[c(v, if (v == length(b)) 1L else v + 1L)]
      if (i == j && any(e %in% f)) next
      if (segments_cross_3d(xy[e[1L], ], xy[e[2L], ], xy[f[1L], ], xy[f[2L], ], tolerance)) {
        stop("Polygon rings must be simple and nonintersecting.", call. = FALSE)
      }
    }
  }
  if (any(vapply(rings, function(r) abs(ring_area_3d(xy, r)) <= tolerance, logical(1)))) stop("Polygon rings must have nonzero area.", call. = FALSE)
  depth <- vapply(seq_along(rings), function(i) sum(vapply(setdiff(seq_along(rings), i), function(j) inside_ring_3d(xy[rings[[i]][1L], ], xy, rings[[j]]), logical(1))), integer(1))
  result <- list()
  for (i in which(depth %% 2L == 0L)) {
    outer <- rings[[i]]
    if (ring_area_3d(xy, outer) < 0) outer <- rev(outer)
    holes <- which(depth == depth[i] + 1L & vapply(rings, function(r) inside_ring_3d(xy[r[1L], ], xy, outer), logical(1)))
    for (j in holes) {
      hole <- rings[[j]]
      if (ring_area_3d(xy, hole) > 0) hole <- rev(hole)
      outer <- bridge_ring_3d(outer, hole, xy, rings, tolerance)
    }
    result <- c(result, ear_triangles_3d(xy, outer, tolerance))
  }
  list(triangles = result, rings = rings)
}

emit_polygons_3d <- function(data, params, panel_params) {
  valid <- valid_rows_3d(data, params)
  primitives <- list(); kinds <- character()
  for (group in unique(data$group)) {
    rows <- which(data$group == group)
    if (!all(valid[rows])) next
    feature <- data[rows, , drop = FALSE]
    ring_id <- feature$subgroup %or3d% rep.int(1L, nrow(feature))
    topology <- triangulate_polygon_3d(feature, split(seq_len(nrow(feature)), ring_id))
    faces <- lapply(topology$triangles, function(i) rows[i])
    if (!all(is.na(feature$fill))) {
      primitives <- c(primitives, faces); kinds <- c(kinds, rep("faces", length(faces)))
    }
    if (!all(is.na(feature$colour))) {
      boundaries <- lapply(topology$rings, function(i) rows[c(i, i[1L])])
      primitives <- c(primitives, boundaries); kinds <- c(kinds, rep("polylines", length(boundaries)))
    }
  }
  packet_from_primitives_3d(data, primitives, kinds, params, panel_params$panel_id)
}

register_geom_3d(ggplot2::GeomPolygon, emit_polygons_3d)
