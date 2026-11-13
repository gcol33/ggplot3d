utils::globalVariables("geometry_triangulate_rings_cpp")

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

triangulate_polygon_3d <- function(data, rings, strict = TRUE) {
  xyz <- as.matrix(data[c("x", "y", "z")])
  centred <- sweep(xyz, 2L, colMeans(xyz))
  decomposition <- svd(centred, nu = 0L)
  tolerance <- 256 * .Machine$double.eps * max(1, max(abs(centred)))
  if (length(decomposition$d) < 2L || decomposition$d[2L] <= tolerance) {
    if (strict) stop("Polygon vertices are collinear.", call. = FALSE)
    return(list(triangles = list(), rings = list()))
  }
  if (length(decomposition$d) >= 3L && decomposition$d[3L] > tolerance * sqrt(nrow(data))) stop("Polygon rings must be planar after scale transformation.", call. = FALSE)
  xy <- centred %*% decomposition$v[, 1:2, drop = FALSE]
  rings <- lapply(rings, function(r) {
    if (length(r) > 3L && all(xyz[r[1L], ] == xyz[r[length(r)], ])) r <- r[-length(r)]
    if (!strict) {
      repeated <- rowSums(abs(xyz[r, , drop = FALSE] - xyz[c(r[-1L], r[1L]), , drop = FALSE])) == 0
      return(r[!repeated])
    }
    if (length(r) < 3L || anyDuplicated(as.data.frame(xyz[r, , drop = FALSE]))) stop("Polygon rings require at least three distinct vertices.", call. = FALSE)
    r
  })
  if (!strict) rings <- rings[lengths(rings) >= 3L]
  if (!length(rings)) return(list(triangles = list(), rings = rings))
  t <- triangulate_rings_2d(xy, rings, tolerance, strict)
  list(triangles = lapply(seq_len(nrow(t)), function(i) t[i, ]), rings = rings)
}

# Triangles (rows of a matrix of row indices of xy) filling the region inside an odd
# number of rings, counterclockwise in xy; triangles without area are dropped.
triangulate_rings_2d <- function(xy, rings, tolerance = 0, strict = FALSE) {
  order <- unlist(rings, use.names = FALSE)
  t <- geometry_triangulate_rings_cpp(xy[order, , drop = FALSE], rep.int(seq_along(rings), lengths(rings)), tolerance, strict)
  t[] <- order[t]
  a <- xy[t[, 1L], , drop = FALSE]
  area <- (xy[t[, 2L], 1L] - a[, 1L]) * (xy[t[, 3L], 2L] - a[, 2L]) - (xy[t[, 2L], 2L] - a[, 2L]) * (xy[t[, 3L], 1L] - a[, 1L])
  t[area > tolerance, , drop = FALSE]
}

emit_polygons_3d <- function(data, params, panel_params, strict = TRUE) {
  valid <- valid_rows_3d(data, params)
  primitives <- vector("list", length(unique(data$group)))
  kinds <- counts <- primitives
  feature_id <- 0L
  for (group in unique(data$group)) {
    rows <- which(data$group == group)
    if (!all(valid[rows])) next
    feature <- data[rows, , drop = FALSE]
    ring_id <- feature$subgroup %or3d% rep.int(1L, nrow(feature))
    topology <- triangulate_polygon_3d(feature, split(seq_len(nrow(feature)), ring_id), strict)
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
