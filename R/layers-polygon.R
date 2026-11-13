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
  t <- triangulate_rings_2d(xy, rings, tolerance)
  list(triangles = lapply(seq_len(nrow(t)), function(i) t[i, ]), rings = rings)
}

# Triangles (rows of a matrix of row indices of xy) filling the region inside an odd
# number of rings, counterclockwise in xy; triangles without area are dropped.
triangulate_rings_2d <- function(xy, rings, tolerance = 0) {
  order <- unlist(rings, use.names = FALSE)
  t <- geometry_triangulate_rings_cpp(xy[order, , drop = FALSE], rep.int(seq_along(rings), lengths(rings)))
  t[] <- order[t]
  a <- xy[t[, 1L], , drop = FALSE]
  area <- (xy[t[, 2L], 1L] - a[, 1L]) * (xy[t[, 3L], 2L] - a[, 2L]) - (xy[t[, 2L], 2L] - a[, 2L]) * (xy[t[, 3L], 1L] - a[, 1L])
  t[area > tolerance, , drop = FALSE]
}

emit_polygons_3d <- function(data, params, panel_params) {
  valid <- valid_rows_3d(data, params)
  primitives <- vector("list", length(unique(data$group)))
  kinds <- counts <- primitives
  feature_id <- 0L
  for (group in unique(data$group)) {
    rows <- which(data$group == group)
    if (!all(valid[rows])) next
    feature <- data[rows, , drop = FALSE]
    ring_id <- feature$subgroup %or3d% rep.int(1L, nrow(feature))
    topology <- triangulate_polygon_3d(feature, split(seq_len(nrow(feature)), ring_id))
    feature_id <- feature_id + 1L
    faces <- rows[unlist(topology$triangles, use.names = FALSE)]
    vertices <- integer(); sizes <- integer(); types <- character()
    if (!all(is.na(feature$fill))) {
      vertices <- faces
      sizes <- rep.int(3L, length(topology$triangles))
      types <- rep("faces", length(sizes))
    }
    if (!all(is.na(feature$colour))) {
      boundaries <- lapply(topology$rings, function(i) rows[c(i, i[1L])])
      vertices <- c(vertices, unlist(boundaries, use.names = FALSE))
      sizes <- c(sizes, lengths(boundaries))
      types <- c(types, rep("polylines", length(boundaries)))
    }
    primitives[[feature_id]] <- vertices
    counts[[feature_id]] <- sizes
    kinds[[feature_id]] <- types
  }
  packet_from_primitives_3d(data, unlist(primitives, use.names = FALSE), unlist(kinds, use.names = FALSE),
                            params, panel_params$panel_id, unlist(counts, use.names = FALSE))
}

register_geom_3d(ggplot2::GeomPolygon, emit_polygons_3d)
