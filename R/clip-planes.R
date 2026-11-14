#' Clipping planes
#'
#' A clipping plane cuts the layers of a 3D panel and keeps the half-space its
#' normal points into. Give a list of them to `coord_3d(clip_planes = )` to cut
#' every layer, or to the `clip_planes` argument of a layer to replace the
#' coordinate system's list for that layer; `clip_planes = list()` leaves a
#' layer uncut.
#'
#' The plane is set in data units and passed through the position scales, so
#' it is a plane in the space the scales draw, as in Makie's `clip_planes` and
#' rgl's `clipplanes3d()`. Faces are cut exactly, lines are cut where they
#' cross the plane, and points and text are kept or dropped by their anchor.
#' Panes, grid lines and axes are not cut. With `cap = TRUE`, the cross-section
#' of each closed surface cut by the plane is closed by a face on the plane,
#' like MATLAB's `isocaps()` and VTK's `vtkClipClosedSurface`. Each closed
#' cut outline is filled, less the outlines nested inside it, so a hollow
#' solid keeps its hole; the cap takes the style of the faces around it.
#' Surfaces that are not closed leave open outlines and get no cap. Planes apply in order, so a later plane also cuts
#' the caps of earlier ones.
#' @param normal Three numbers, the normal of the plane along x, y and z in
#'   the units of the transformed position scales. The plane keeps the side
#'   the normal points into.
#' @param point A point on the plane in data units: three numbers, or a named
#'   vector or list giving the coordinates along which the normal is nonzero,
#'   such as `c(x = 5)` for the plane `x = 5`. `NULL` takes the centre of the
#'   panel box.
#' @param cap Whether to close cut solids with a face on the plane.
#' @return A clipping plane for [coord_3d()] or a layer's `clip_planes`.
#' @seealso [geom_plane_3d()] draws a plane.
#' @examples
#' clip_plane_3d(normal = c(1, 0, 0), point = c(x = 0))
#' d <- expand.grid(x = seq(-2, 2, length.out = 30), y = seq(-2, 2, length.out = 30))
#' d$z <- exp(-(d$x^2 + d$y^2))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(fill = "steelblue") +
#'   coord_3d(clip_planes = list(clip_plane_3d(c(1, 1, 0), c(0, 0, 0))))
#' ggplot2::ggplot(data.frame(x = 0, y = 0, z = 0), ggplot2::aes(x, y, z = z)) +
#'   geom_sphere_3d(radius = 1, fill = "tomato") +
#'   coord_3d(xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-1, 1),
#'     clip_planes = list(clip_plane_3d(c(0, -1, 0), c(y = 0.3), cap = TRUE)))
#' @export
clip_plane_3d <- function(normal, point = NULL, cap = FALSE) {
  if (!is.logical(cap) || length(cap) != 1L || is.na(cap)) stop("cap must be TRUE or FALSE.", call. = FALSE)
  structure(c(plane_spec_3d(normal, point), list(cap = cap)), class = "ggplot3d_clip_plane")
}

#' @export
print.ggplot3d_clip_plane <- function(x, ...) {
  point <- if (is.null(x$point)) "the box centre" else
    paste0("(", paste(vapply(x$point, function(v) if (is.null(v)) "." else format(v), character(1)), collapse = ", "), ")")
  cat("<clip_plane_3d> normal (", paste(format(x$normal), collapse = ", "), ") through ", point,
      if (x$cap) ", capped", "\n", sep = "")
  invisible(x)
}

# Normal and point of a plane in data units. The point is a list over x, y, z
# with NULL where the normal component is zero and no value was given, or
# NULL for the centre of the panel box.
plane_spec_3d <- function(normal, point = NULL) {
  axes <- c("x", "y", "z")
  if (!is.numeric(normal) || length(normal) != 3L || !all(is.finite(normal)) || sum(normal^2) == 0) {
    stop("normal must be three finite numbers, not all zero.", call. = FALSE)
  }
  if (!is.null(names(normal))) {
    if (!setequal(names(normal), axes)) stop("A named normal must name x, y and z.", call. = FALSE)
    normal <- normal[axes]
  }
  if (is.null(point)) return(list(normal = as.double(unname(normal)), point = NULL))
  if (!(is.atomic(point) || is.list(point)) || !length(point)) stop("point must be a vector or list of coordinates.", call. = FALSE)
  if (is.null(names(point))) {
    if (length(point) != 3L) stop("An unnamed point must have three coordinates.", call. = FALSE)
    names(point) <- axes
  }
  if (!all(names(point) %in% axes) || anyDuplicated(names(point))) stop("point must be named by x, y and z.", call. = FALSE)
  coordinates <- stats::setNames(lapply(axes, function(a) {
    if (a %in% names(point)) point[[a]] else NULL
  }), axes)
  missing <- axes[normal != 0 & vapply(coordinates, is.null, logical(1))]
  if (length(missing)) stop("point must give the ", paste(missing, collapse = " and "), " coordinate, along which the normal is nonzero.", call. = FALSE)
  for (a in axes) {
    v <- coordinates[[a]]
    if (!is.null(v) && (length(v) != 1L || is.na(v))) stop("Each point coordinate must be one value.", call. = FALSE)
  }
  list(normal = as.double(unname(normal)), point = coordinates)
}

# Position of one data value of an axis in the panel box, from 0 to 1.
scale_unit_3d <- function(axis, value, panel_params) {
  range <- panel_params$ranges[[axis]]
  if (is.numeric(value) && is.infinite(value)) return(as.double(value > 0))
  view <- panel_params[[axis]]
  position <- if (view$is_discrete()) as.numeric(value) else view$get_transformation()$transform(value)
  unit <- (as.numeric(position) - range[1L]) / diff(range)
  if (!is.finite(unit)) stop("The ", axis, " value ", format(value), " is undefined on this scale.", call. = FALSE)
  unit
}

# A plane in panel box coordinates u: normal m and offset with m . u + offset
# >= 0 on the kept side. A data normal n becomes n * (scale range widths).
unit_plane_3d <- function(spec, panel_params, cap = FALSE) {
  axes <- c("x", "y", "z")
  widths <- vapply(axes, function(a) diff(panel_params$ranges[[a]]), numeric(1))
  m <- spec$normal * widths
  p <- if (is.null(spec$point)) rep(0.5, 3L) else
    vapply(axes, function(a) if (is.null(spec$point[[a]])) 0 else scale_unit_3d(a, spec$point[[a]], panel_params), numeric(1))
  m <- unname(m / sqrt(sum(m^2)))
  list(normal = m, offset = -sum(m * p), cap = isTRUE(cap))
}

# Orthonormal in-plane directions u and v (columns) of a unit normal n, with
# u x v = n, so rings counterclockwise in (u, v) face along n.
plane_basis_3d <- function(normal) {
  helper <- if (abs(normal[1L]) < 0.9) c(1, 0, 0) else c(0, 1, 0)
  u <- helper - sum(helper * normal) * normal
  u <- u / sqrt(sum(u^2))
  v <- c(normal[2L] * u[3L] - normal[3L] * u[2L], normal[3L] * u[1L] - normal[1L] * u[3L], normal[1L] * u[2L] - normal[2L] * u[1L])
  cbind(u, v, deparse.level = 0)
}

# The six faces of the panel box as planes keeping its inside.
unit_box_planes_3d <- function() {
  planes <- list()
  for (k in 1:3) for (side in 0:1) {
    normal <- numeric(3); normal[k] <- if (side == 0L) 1 else -1
    planes[[length(planes) + 1L]] <- list(normal = normal, offset = side, cap = FALSE)
  }
  planes
}

# Clipping planes of a layer in panel box coordinates: the layer's own list,
# or the coordinate system's when the layer gives none.
layer_clip_planes_3d <- function(planes, panel_params) {
  planes <- planes %or3d% panel_params$coord_params$clip_planes
  if (is.null(planes)) return(list())
  if (inherits(planes, "ggplot3d_clip_plane")) planes <- list(planes)
  check_clip_planes_3d(planes)
  lapply(planes, function(p) unit_plane_3d(p, panel_params, p$cap))
}

check_clip_planes_3d <- function(planes) {
  if (inherits(planes, "ggplot3d_clip_plane")) planes <- list(planes)
  if (!is.list(planes) || !all(vapply(planes, inherits, logical(1), "ggplot3d_clip_plane"))) {
    stop("clip_planes must be a list of clip_plane_3d() planes.", call. = FALSE)
  }
  invisible(planes)
}

# Whether points lie on the kept side of every plane.
inside_planes_3d <- function(xyz, planes) {
  keep <- rep(TRUE, nrow(xyz))
  for (plane in planes) keep <- keep & as.vector(xyz %*% plane$normal) + plane$offset >= 0
  keep
}

# Packet clipping ---------------------------------------------------------------

# Cuts a packet by planes in packet coordinates, in order. Faces are clipped
# exactly, polylines split into the runs on the kept side, markers and text
# kept by their anchor. Planes with cap = TRUE close the closed surfaces they
# cut. The result's "parent" attribute maps each primitive to the input row of
# its kind it came from; a cap maps to a face around it.
clip_packet_planes_3d <- function(packet, planes) {
  kinds <- c("faces", "polylines", "markers", "text")
  parent <- lapply(kinds, function(kind) seq_along(packet[[kind]]$offset))
  names(parent) <- kinds
  for (plane in planes) {
    packet <- clip_packet_plane_3d(packet, plane)
    step <- attr(packet, "parent")
    for (kind in kinds) parent[[kind]] <- parent[[kind]][step[[kind]]]
  }
  attr(packet, "parent") <- parent
  packet
}

clip_packet_plane_3d <- function(packet, plane, eps = 1e-12) {
  xyzw <- scene_vertex_matrix_3d(packet$vertices)
  distance <- as.vector(xyzw[, 1:3, drop = FALSE] %*% plane$normal) + plane$offset * xyzw[, 4L]
  distance[abs(distance) < eps] <- 0
  parent <- list()
  faces <- clip_faces_plane_3d(packet, distance, plane)
  packet <- faces$packet; parent$faces <- faces$parent
  lines <- clip_polylines_plane_3d(packet, distance, plane)
  packet <- lines$packet; parent$polylines <- lines$parent
  for (kind in c("markers", "text")) {
    tab <- packet[[kind]]
    keep <- which(distance[packet$corners$vertex[tab$offset]] >= 0)
    packet[[kind]] <- lapply(tab, function(column) column[keep])
    parent[[kind]] <- keep
  }
  if (plane$cap && length(faces$cut$face)) {
    capped <- cap_packet_3d(packet, faces$cut, plane)
    packet <- capped$packet
    parent$faces <- c(parent$faces, capped$source)
  }
  next_id <- 0L
  for (kind in c("faces", "polylines", "markers", "text")) {
    n <- length(packet[[kind]]$offset)
    packet[[kind]]$primitive_id <- as.integer(next_id + seq_len(n))
    next_id <- next_id + n
  }
  attr(packet, "parent") <- parent
  packet
}

# Points where edges cross the plane, one per edge, as homogeneous rows
# (x, y, z, w): the crossing is linear in (x, y, z, w), so edges through
# infinity are cut before any division. The edge is oriented by its endpoint
# coordinates, so faces sharing an edge get the same point. Axis-aligned planes
# put the point exactly on the plane.
edge_crossings_3d <- function(xyz, a, b, distance, plane) {
  pa <- xyz[a, , drop = FALSE]; pb <- xyz[b, , drop = FALSE]
  swap <- pa[, 1L] > pb[, 1L] | (pa[, 1L] == pb[, 1L] & (pa[, 2L] > pb[, 2L] | (pa[, 2L] == pb[, 2L] & pa[, 3L] > pb[, 3L])))
  lo <- ifelse(swap, b, a); hi <- ifelse(swap, a, b)
  t <- distance[lo] / (distance[lo] - distance[hi])
  point <- xyz[lo, , drop = FALSE] + t * (xyz[hi, , drop = FALSE] - xyz[lo, , drop = FALSE])
  axis <- which(plane$normal != 0)
  if (length(axis) == 1L) point[, axis] <- -plane$offset * point[, 4L] / plane$normal[axis]
  point
}

append_vertices_3d <- function(packet, points) {
  start <- length(packet$vertices$x)
  added <- scene_vertices_3d(points)
  packet$vertices <- Map(c, packet$vertices, added[names(packet$vertices)])
  list(packet = packet, ids = start + seq_len(nrow(points)))
}

# Appends corners. Optional corner attributes come from corner `from`, or lie
# between corners `from` and `to` at the homogeneous parameter t, interpolated
# affinely in the Euclidean point.
append_corners_3d <- function(packet, vertex, source_row, from, to = from, t = 0) {
  corners <- packet$corners
  start <- length(corners$vertex)
  extra <- setdiff(names(corners), c("vertex", "source_row"))
  if (length(extra)) {
    pa <- (1 - t) * packet$vertices$w[corners$vertex[from]]
    pb <- t * packet$vertices$w[corners$vertex[to]]
    beta <- ifelse(pa + pb > 0, pb / (pa + pb), t)
    for (name in extra) corners[[name]] <- c(corners[[name]],
      (1 - beta) * corners[[name]][from] + beta * corners[[name]][to])
  }
  corners$vertex <- c(corners$vertex, as.integer(vertex))
  corners$source_row <- c(corners$source_row, as.integer(source_row))
  packet$corners <- corners[c("vertex", "source_row", extra)]
  list(packet = packet, start = start)
}

# Sutherland-Hodgman clipping of the convex faces that cross the plane. The
# cut record lists, for every face with a corner on the removed side, the
# points it has on the plane, for capping.
clip_faces_plane_3d <- function(packet, distance, plane) {
  faces <- packet$faces
  n <- length(faces$offset)
  empty_cut <- list(face = integer(), faces = faces, xyz = matrix(numeric(), 0L, 4L))
  if (!n) return(list(packet = packet, parent = integer(), cut = empty_cut))
  face <- rep.int(seq_len(n), faces$count)
  position <- sequence(faces$count)
  corner <- faces$offset[face] + position - 1L
  following <- faces$offset[face] + position %% faces$count[face]
  vertex <- packet$corners$vertex[corner]; next_vertex <- packet$corners$vertex[following]
  d <- distance[vertex]; dn <- distance[next_vertex]
  negative <- tabulate(face[d < 0], n); positive <- tabulate(face[d > 0], n)
  if (!any(negative > 0)) return(list(packet = packet, parent = seq_len(n), cut = empty_cut))
  crossing <- (d > 0 & dn < 0) | (d < 0 & dn > 0)
  xyz <- scene_vertex_matrix_3d(packet$vertices)
  edge_key <- paste(pmin(vertex, next_vertex), pmax(vertex, next_vertex))
  crossed <- which(crossing & !duplicated(ifelse(crossing, edge_key, NA_character_), incomparables = NA))
  points <- edge_crossings_3d(xyz, vertex[crossed], next_vertex[crossed], distance, plane)
  added <- append_vertices_3d(packet, points)
  packet <- added$packet
  crossing_vertex <- integer(length(d))
  crossing_vertex[crossing] <- added$ids[match(edge_key[crossing], edge_key[crossed])]
  cut_faces <- negative > 0 & positive > 0
  rows <- which(cut_faces[face])
  keep_vertex <- rows[d[rows] >= 0]; keep_crossing <- rows[crossing[rows]]
  entries <- c(keep_vertex, keep_crossing)
  out_vertex <- c(vertex[keep_vertex], crossing_vertex[keep_crossing])
  out_to <- c(corner[keep_vertex], following[keep_crossing])
  out_t <- c(numeric(length(keep_vertex)), d[keep_crossing] / (d[keep_crossing] - dn[keep_crossing]))
  ordering <- order(c(2L * keep_vertex, 2L * keep_crossing + 1L))
  entries <- entries[ordering]; out_vertex <- out_vertex[ordering]
  out_to <- out_to[ordering]; out_t <- out_t[ordering]
  owner <- face[entries]
  appended <- append_corners_3d(packet, out_vertex, packet$corners$source_row[corner[entries]],
    corner[entries], out_to, out_t)
  packet <- appended$packet
  counts <- tabulate(owner, n)
  starts <- appended$start + match(seq_len(n), owner)
  faces$offset[cut_faces] <- as.integer(starts[cut_faces])
  faces$count[cut_faces] <- as.integer(counts[cut_faces])
  kept <- which(negative == 0L | (cut_faces & faces$count >= 3L))
  on_plane <- (negative > 0)[face] & (d == 0 | crossing)
  cut <- list(face = face[on_plane], faces = faces,
    xyz = scene_vertex_matrix_3d(packet$vertices)[ifelse(d[on_plane] == 0, vertex[on_plane], crossing_vertex[on_plane]), , drop = FALSE])
  faces$primitive_id <- seq_len(n)
  packet$faces <- lapply(faces, function(column) column[kept])
  packet <- clean_flat_faces_3d(packet)
  list(packet = packet, parent = packet$faces$primitive_id, cut = cut)
}

# Polylines keep their runs of corners on the kept side, ending at the plane.
clip_polylines_plane_3d <- function(packet, distance, plane) {
  line <- packet$polylines
  n <- length(line$offset)
  if (!n) return(list(packet = packet, parent = integer()))
  owner <- rep.int(seq_len(n), line$count)
  position <- sequence(line$count)
  corner <- line$offset[owner] + position - 1L
  d <- distance[packet$corners$vertex[corner]]
  touched <- tabulate(owner[d < 0], n) > 0L
  if (!any(touched)) return(list(packet = packet, parent = seq_len(n)))
  rows <- which(touched[owner])
  last <- position[rows] == line$count[owner[rows]]
  dn <- d[pmin(rows + 1L, length(d))]
  crossing <- !last & ((d[rows] > 0 & dn < 0) | (d[rows] < 0 & dn > 0))
  vertex <- packet$corners$vertex[corner[rows]]
  next_vertex <- packet$corners$vertex[corner[pmin(rows + 1L, length(d))]]
  xyz <- scene_vertex_matrix_3d(packet$vertices)
  added <- append_vertices_3d(packet, edge_crossings_3d(xyz, vertex[crossing], next_vertex[crossing], distance, plane))
  packet <- added$packet
  index <- seq_along(rows)
  key <- c(2L * index, 2L * index[crossing] + 1L)
  kind <- c(ifelse(d[rows] >= 0, 1L, 0L), rep.int(2L, sum(crossing)))
  ref_vertex <- c(vertex, added$ids)
  ref_owner <- c(owner[rows], owner[rows][crossing])
  ref_source <- packet$corners$source_row[corner[c(rows, rows[crossing])]]
  ref_from <- corner[c(rows, rows[crossing])]
  ref_to <- c(corner[rows], corner[pmin(rows + 1L, length(d))][crossing])
  ref_t <- c(numeric(length(rows)), d[rows][crossing] / (d[rows][crossing] - dn[crossing]))
  ordering <- order(key)
  kind <- kind[ordering]; ref_vertex <- ref_vertex[ordering]; ref_owner <- ref_owner[ordering]; ref_source <- ref_source[ordering]
  ref_from <- ref_from[ordering]; ref_to <- ref_to[ordering]; ref_t <- ref_t[ordering]
  run <- cumsum(kind == 0L | c(TRUE, ref_owner[-1L] != ref_owner[-length(ref_owner)]))
  emitted <- kind != 0L
  run <- run[emitted]
  sizes <- tabulate(match(run, unique(run)))
  long <- unique(run)[sizes >= 2L]
  emitted_rows <- which(emitted)[run %in% long]
  run <- run[run %in% long]
  appended <- append_corners_3d(packet, ref_vertex[emitted_rows], ref_source[emitted_rows],
    ref_from[emitted_rows], ref_to[emitted_rows], ref_t[emitted_rows])
  packet <- appended$packet
  run_owner <- ref_owner[emitted_rows][!duplicated(run)]
  run_count <- tabulate(match(run, unique(run)))
  run_offset <- appended$start + match(unique(run), run)
  kept <- which(!touched)
  parent <- c(kept, run_owner)
  out <- lapply(line, function(column) column[parent])
  out$offset[seq_along(kept)] <- line$offset[kept]
  out$count[seq_along(kept)] <- line$count[kept]
  if (length(run_owner)) {
    out$offset[length(kept) + seq_along(run_owner)] <- as.integer(run_offset)
    out$count[length(kept) + seq_along(run_owner)] <- as.integer(run_count)
  }
  order_out <- order(parent, seq_along(parent))
  packet$polylines <- lapply(out, function(column) column[order_out])
  list(packet = packet, parent = parent[order_out])
}

# Closes cut surfaces with faces on the plane: every removed face with two
# points on the plane gives one segment of the cut outline; segments chain
# into closed rings, which are triangulated by nested regions. Each region
# takes the style of the faces most of its outer ring was cut from.
cap_packet_3d <- function(packet, cut, plane) {
  none <- list(packet = packet, source = integer())
  cut_xyz <- homogeneous_xyz_3d(cut$xyz)
  key <- paste(sprintf("%.17g", cut_xyz[, 1L]), sprintf("%.17g", cut_xyz[, 2L]), sprintf("%.17g", cut_xyz[, 3L]))
  point <- match(key, unique(key))
  xyz <- cut_xyz[!duplicated(key), , drop = FALSE]
  pairs <- unique(data.frame(face = cut$face, point = point))
  sizes <- tabulate(pairs$face, max(c(0L, pairs$face)))
  pairs <- pairs[sizes[pairs$face] == 2L, , drop = FALSE]
  if (!nrow(pairs)) return(none)
  pairs <- pairs[order(pairs$face, pairs$point), , drop = FALSE]
  a <- pairs$point[c(TRUE, FALSE)]; b <- pairs$point[c(FALSE, TRUE)]
  segment_face <- pairs$face[c(TRUE, FALSE)]
  loops <- chain_segments_3d(a, b, nrow(xyz))
  if (!length(loops$rings)) return(none)
  basis <- plane_basis_3d(plane$normal)
  xy <- xyz %*% basis
  rings <- loops$rings
  ring_face <- vapply(loops$segments, function(s) {
    faces <- segment_face[s]
    styles <- cut$faces$style_id[faces]
    common <- as.integer(names(which.max(table(styles))))
    faces[match(common, styles)]
  }, integer(1))
  groups <- nested_ring_groups_3d(xy, rings)
  pieces <- lapply(groups, function(g) triangulate_rings_2d(xy, rings[g], 1e-14))
  triangles <- do.call(rbind, pieces)
  if (is.null(triangles) || !nrow(triangles)) return(none)
  source <- rep(ring_face[vapply(groups, `[`, integer(1), 1L)], vapply(pieces, nrow, integer(1)))
  added <- append_vertices_3d(packet, xyz)
  packet <- added$packet
  corner_vertex <- added$ids[as.vector(t(triangles[, 3:1, drop = FALSE]))]
  appended <- append_corners_3d(packet, corner_vertex, rep(cut$faces$source_row[source], each = 3L),
    rep(cut$faces$offset[source], each = 3L))
  packet <- appended$packet
  # A cap corner faces out of the kept side, against the plane normal.
  if (!is.null(packet$corners$normal_x)) {
    added <- appended$start + seq_along(corner_vertex)
    outward <- -plane$normal / sqrt(sum(plane$normal^2))
    for (k in 1:3) packet$corners[[c("normal_x", "normal_y", "normal_z")[k]]][added] <- outward[k]
  }
  styles <- packet$styles
  style <- cut$faces$style_id[source]
  normals <- c("normal_x", "normal_y", "normal_z")
  if (all(normals %in% names(styles))) {
    used <- unique(style)
    copies <- styles[used, , drop = FALSE]
    copies[normals] <- NA_real_
    copies$style_id <- nrow(styles) + seq_along(used)
    packet$styles <- rbind(styles, copies)
    packet$style_count <- nrow(packet$styles)
    style <- copies$style_id[match(style, used)]
  }
  caps <- lapply(cut$faces, function(column) column[source])
  caps$offset <- as.integer(appended$start + 3L * seq_along(source) - 2L)
  caps$count <- rep.int(3L, length(source))
  caps$style_id <- as.integer(style)
  packet$faces <- Map(c, packet$faces, caps)
  list(packet = packet, source = source)
}

# Closed rings of points from undirected segments a-b; open chains are dropped.
chain_segments_3d <- function(a, b, n_points) {
  n <- length(a)
  incident <- split(c(seq_len(n), seq_len(n)), factor(c(a, b), levels = seq_len(n_points)))
  used <- logical(n)
  rings <- list(); segments <- list()
  for (s in seq_len(n)) {
    if (used[s]) next
    used[s] <- TRUE
    start <- a[s]; current <- b[s]
    ring <- integer(n + 1L); ring[1L] <- start; size <- 1L
    members <- s
    closed <- FALSE
    repeat {
      if (current == start) { closed <- TRUE; break }
      size <- size + 1L; ring[size] <- current
      candidates <- incident[[current]]
      candidates <- candidates[!used[candidates]]
      if (!length(candidates)) break
      s2 <- candidates[1L]
      used[s2] <- TRUE
      members <- c(members, s2)
      current <- if (a[s2] == current) b[s2] else a[s2]
    }
    if (closed && size >= 3L) {
      rings[[length(rings) + 1L]] <- ring[seq_len(size)]
      segments[[length(segments) + 1L]] <- members
    }
  }
  list(rings = rings, segments = segments)
}

# Rings grouped into regions: each ring nested inside an even number of
# others is an outer ring, grouped with the rings one level deeper inside it.
# A ring counts as inside another when all its points are; rings that cross,
# such as the outlines of two overlapping solids, are separate regions.
nested_ring_groups_3d <- function(xy, rings) {
  n <- length(rings)
  lo <- t(vapply(rings, function(r) apply(xy[r, , drop = FALSE], 2L, min), numeric(2)))
  hi <- t(vapply(rings, function(r) apply(xy[r, , drop = FALSE], 2L, max), numeric(2)))
  inside <- outer(seq_len(n), seq_len(n), function(i, j) i != j & lo[i, 1L] <= lo[j, 1L] & lo[i, 2L] <= lo[j, 2L] &
    hi[j, 1L] <= hi[i, 1L] & hi[j, 2L] <= hi[i, 2L])
  for (pair in which(inside)) {
    i <- (pair - 1L) %% n + 1L; j <- (pair - 1L) %/% n + 1L
    inside[i, j] <- all(inside_ring_points_3d(xy[rings[[j]], , drop = FALSE], xy, rings[[i]]))
  }
  depth <- colSums(inside)
  lapply(which(depth %% 2L == 0L), function(i) c(i, which(inside[i, ] & depth == depth[i] + 1L)))
}

# Even-odd point-in-ring test for many points.
inside_ring_points_3d <- function(points, xy, ring) {
  inside <- logical(nrow(points))
  px <- points[, 1L]; py <- points[, 2L]
  j <- ring[c(length(ring), seq_len(length(ring) - 1L))]
  for (k in seq_along(ring)) {
    xk <- xy[ring[k], 1L]; yk <- xy[ring[k], 2L]; xj <- xy[j[k], 1L]; yj <- xy[j[k], 2L]
    flip <- (yk > py) != (yj > py)
    if (any(flip)) inside[flip] <- xor(inside[flip], px[flip] < (xj - xk) * (py[flip] - yk) / (yj - yk) + xk)
  }
  inside
}
