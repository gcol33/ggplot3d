# Instanced glyphs: one unit mesh placed at many centres, each through its own
# 3 x 3 linear map. A unit mesh is list(vertices = V x 3 matrix, faces = list of
# index matrices grouped by corner count, edges = E x 2 matrix, sphere = TRUE
# when the vertices lie on the unit sphere). Faces wind counterclockwise seen
# from outside.

.glyph_cache_3d <- new.env(parent = emptyenv())

glyph_names_3d <- c("sphere", "cube", "cone", "cylinder", "octahedron")

glyph_mesh_edges_3d <- function(faces) {
  edges <- do.call(rbind, lapply(faces, function(f) {
    k <- ncol(f)
    cbind(as.vector(f), as.vector(f[, c(seq.int(2L, k), 1L), drop = FALSE]))
  }))
  edges[!duplicated(cbind(pmin(edges[, 1L], edges[, 2L]), pmax(edges[, 1L], edges[, 2L]))), , drop = FALSE]
}

new_glyph_mesh_3d <- function(vertices, faces, sphere = FALSE) {
  faces <- lapply(faces, function(f) matrix(as.integer(f), ncol = ncol(f)))
  faces <- faces[vapply(faces, nrow, integer(1)) > 0L]
  list(vertices = unname(vertices), faces = faces, edges = glyph_mesh_edges_3d(faces), sphere = sphere)
}

# Convex glyphs centred on the origin: each face turns away from the centre.
orient_outward_3d <- function(vertices, faces) {
  lapply(faces, function(f) {
    a <- vertices[f[, 1L], , drop = FALSE]
    u <- vertices[f[, 2L], , drop = FALSE] - a; v <- vertices[f[, 3L], , drop = FALSE] - a
    normal <- cross_rows_3d(u, v)
    centroid <- Reduce(`+`, lapply(seq_len(ncol(f)), function(j) vertices[f[, j], , drop = FALSE])) / ncol(f)
    flip <- rowSums(normal * centroid) < 0
    f[flip, ] <- f[flip, rev(seq_len(ncol(f))), drop = FALSE]
    f
  })
}

# Icosahedron subdivided `detail` times, each new vertex pushed onto the sphere.
icosphere_3d <- function(detail) {
  t <- (1 + sqrt(5)) / 2
  v <- rbind(c(-1, t, 0), c(1, t, 0), c(-1, -t, 0), c(1, -t, 0), c(0, -1, t), c(0, 1, t),
             c(0, -1, -t), c(0, 1, -t), c(t, 0, -1), c(t, 0, 1), c(-t, 0, -1), c(-t, 0, 1))
  f <- rbind(c(1, 12, 6), c(1, 6, 2), c(1, 2, 8), c(1, 8, 11), c(1, 11, 12), c(2, 6, 10), c(6, 12, 5),
             c(12, 11, 3), c(11, 8, 7), c(8, 2, 9), c(4, 10, 5), c(4, 5, 3), c(4, 3, 7), c(4, 7, 9),
             c(4, 9, 10), c(5, 10, 6), c(3, 5, 12), c(7, 3, 11), c(9, 7, 8), c(10, 9, 2))
  v <- v / sqrt(rowSums(v^2))
  for (level in seq_len(detail)) {
    n <- nrow(v)
    a <- f[, 1L]; b <- f[, 2L]; c <- f[, 3L]
    key <- function(p, q) pmin(p, q) * (n + 1) + pmax(p, q)
    keys <- c(key(a, b), key(b, c), key(c, a))
    unique_keys <- unique(keys)
    lo <- unique_keys %/% (n + 1); hi <- unique_keys %% (n + 1)
    mid <- v[lo, , drop = FALSE] + v[hi, , drop = FALSE]
    v <- rbind(v, mid / sqrt(rowSums(mid^2)))
    m <- matrix(n + match(keys, unique_keys), ncol = 3L)
    ab <- m[, 1L]; bc <- m[, 2L]; ca <- m[, 3L]
    f <- rbind(cbind(a, ab, ca), cbind(b, bc, ab), cbind(c, ca, bc), cbind(ab, bc, ca))
  }
  new_glyph_mesh_3d(v, orient_outward_3d(v, list(f)), sphere = TRUE)
}

# Circle of n points around the z axis at height z.
ring_3d <- function(n, z) {
  angle <- 2 * pi * (seq_len(n) - 1) / n
  cbind(cos(angle), sin(angle), z)
}

builtin_glyph_3d <- function(name, detail) {
  n <- 8L * 2L^detail
  switch(name,
    sphere = icosphere_3d(detail),
    cube = {
      v <- as.matrix(expand.grid(x = c(-1, 1), y = c(-1, 1), z = c(-1, 1)))
      f <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5), c(3, 7, 8, 4), c(1, 5, 7, 3), c(2, 4, 8, 6))
      new_glyph_mesh_3d(v, orient_outward_3d(v, list(f)))
    },
    octahedron = {
      v <- rbind(c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0), c(0, -1, 0), c(0, 0, 1), c(0, 0, -1))
      f <- rbind(c(1, 3, 5), c(3, 2, 5), c(2, 4, 5), c(4, 1, 5), c(3, 1, 6), c(2, 3, 6), c(4, 2, 6), c(1, 4, 6))
      new_glyph_mesh_3d(v, orient_outward_3d(v, list(f)), sphere = TRUE)
    },
    cone = {
      v <- rbind(ring_3d(n, -1), c(0, 0, 1))
      i <- seq_len(n); j <- c(seq.int(2L, n), 1L)
      new_glyph_mesh_3d(v, orient_outward_3d(v, list(cbind(i, j, n + 1L), matrix(rev(i), 1L))))
    },
    cylinder = {
      v <- rbind(ring_3d(n, -1), ring_3d(n, 1))
      i <- seq_len(n); j <- c(seq.int(2L, n), 1L)
      new_glyph_mesh_3d(v, orient_outward_3d(v, list(cbind(i, j, j + n, i + n), matrix(rev(i), 1L), matrix(i + n, 1L))))
    }
  )
}

# A user mesh keeps its own coordinates: the glyph's origin is the anchor.
mesh_glyph_3d <- function(mesh) {
  corners <- prepare_mesh_data_3d(mesh)
  corners <- corners[order(corners$.mesh, corners$.face, corners$.corner), , drop = FALSE]
  vertex_key <- paste(corners$.mesh, corners$.vertex)
  index <- match(vertex_key, unique(vertex_key))
  vertices <- as.matrix(corners[!duplicated(vertex_key), c("x", "y", "z")])
  if (any(!is.finite(vertices))) stop("Glyph meshes require finite vertex positions.", call. = FALSE)
  face_key <- paste(corners$.mesh, corners$.face)
  rows <- split(seq_len(nrow(corners)), factor(face_key, unique(face_key)))
  triangles <- lapply(rows, function(r) {
    if (length(r) == 3L) return(matrix(index[r], 1L))
    flat <- data.frame(x = vertices[index[r], 1L], y = vertices[index[r], 2L], z = vertices[index[r], 3L])
    topology <- triangulate_polygon_3d(flat, list(seq_along(r)))
    t <- matrix(index[r][unlist(topology$triangles)], ncol = 3L, byrow = TRUE)
    normal <- polygon_normal_3d(vertices[index[r], , drop = FALSE])
    if (sum(cross_rows_3d(vertices[t[1L, 2L], , drop = FALSE] - vertices[t[1L, 1L], , drop = FALSE],
      vertices[t[1L, 3L], , drop = FALSE] - vertices[t[1L, 1L], , drop = FALSE]) * normal) < 0) t <- t[, 3:1, drop = FALSE]
    t
  })
  new_glyph_mesh_3d(vertices, list(do.call(rbind, triangles)))
}

# Newell normal of a polygon ring.
polygon_normal_3d <- function(xyz) {
  nxt <- xyz[c(seq.int(2L, nrow(xyz)), 1L), , drop = FALSE]
  c(sum((xyz[, 2] - nxt[, 2]) * (xyz[, 3] + nxt[, 3])), sum((xyz[, 3] - nxt[, 3]) * (xyz[, 1] + nxt[, 1])),
    sum((xyz[, 1] - nxt[, 1]) * (xyz[, 2] + nxt[, 2])))
}

check_detail_3d <- function(detail) {
  if (!is.numeric(detail) || length(detail) != 1L || !is.finite(detail) || detail < 0 || detail > 6 || detail != trunc(detail)) {
    stop("detail must be a whole number from 0 to 6.", call. = FALSE)
  }
  as.integer(detail)
}

glyph_mesh_3d <- function(glyph = "sphere", detail = 2L) {
  detail <- check_detail_3d(detail)
  if (is.character(glyph)) {
    if (length(glyph) != 1L || !glyph %in% glyph_names_3d) stop("glyph must be one of ", paste(glyph_names_3d, collapse = ", "), " or a mesh.", call. = FALSE)
    key <- paste(glyph, detail)
    if (is.null(.glyph_cache_3d[[key]])) .glyph_cache_3d[[key]] <- builtin_glyph_3d(glyph, detail)
    return(.glyph_cache_3d[[key]])
  }
  if (inherits(glyph, c("ggplot3d_mesh", "mesh3d", "data.frame"))) return(mesh_glyph_3d(glyph))
  stop("glyph must be one of ", paste(glyph_names_3d, collapse = ", "), " or a mesh.", call. = FALSE)
}

# Rotations taking the z axis onto each row of `direction`; rows of zero
# length keep the identity. Returns a 3 x 3 x n array.
align_z_3d <- function(direction) {
  n <- nrow(direction)
  out <- array(diag(3), c(3L, 3L, n))
  length <- sqrt(rowSums(direction^2))
  turn <- which(is.finite(length) & length > 0)
  if (!length(turn)) return(out)
  z <- direction[turn, , drop = FALSE] / length[turn]
  helper <- matrix(c(1, 0, 0), length(turn), 3L, byrow = TRUE)
  helper[abs(z[, 1L]) > 0.9, ] <- matrix(c(0, 1, 0), 1L, 3L)
  x <- cross_rows_3d(helper, z); x <- x / sqrt(rowSums(x^2))
  y <- cross_rows_3d(z, x)
  out[, 1L, turn] <- t(x); out[, 2L, turn] <- t(y); out[, 3L, turn] <- t(z)
  out
}

# Row-wise half-widths along x, y and z of each placed glyph.
glyph_extent_3d <- function(maps, unit) {
  if (unit$sphere) return(t(apply(maps, 3L, function(m) sqrt(rowSums(m^2)))))
  bounds <- apply(maps, 3L, function(m) {
    placed <- unit$vertices %*% t(m)
    c(-apply(placed, 2L, min), apply(placed, 2L, max))
  })
  t(bounds)
}

# Adds xmin/xmax, ymin/ymax and zmin/zmax so data-unit glyphs train the
# scales; rows outside `rows` span only their centre.
glyph_limits_3d <- function(data, maps, unit, rows = rep(TRUE, nrow(data))) {
  lo <- hi <- matrix(0, nrow(data), 3L)
  if (any(rows)) {
    extent <- glyph_extent_3d(maps[, , rows, drop = FALSE], unit)
    if (ncol(extent) == 3L) extent <- cbind(extent, extent)
    extent[!is.finite(extent)] <- 0
    lo[rows, ] <- extent[, 1:3]; hi[rows, ] <- extent[, 4:6]
  }
  for (k in 1:3) {
    axis <- c("x", "y", "z")[k]
    data[[paste0(axis, "min")]] <- data[[axis]] - lo[, k]
    data[[paste0(axis, "max")]] <- data[[axis]] + hi[, k]
  }
  data
}

# Rows of placed instances (glyphs, quadrics) that are drawn: valid positions
# and finite maps; outside the identity frame the anchors are cut by the box
# (with clip_data) and the layer's clip planes, since the instances skip the
# frame.
instance_rows_3d <- function(data, maps, params, panel_params) {
  frame <- frame_of_3d(panel_params)
  keep <- valid_rows_3d(data, params) & apply(maps, 3L, function(m) all(is.finite(m)))
  if (!isTRUE(frame$identity) && isTRUE(panel_params$camera$clip_data)) {
    keep <- keep & data$x >= 0 & data$x <= 1 & data$y >= 0 & data$y <= 1 & data$z >= 0 & data$z <= 1
  }
  if (!isTRUE(frame$identity)) keep <- keep & inside_planes_3d(as.matrix(data[c("x", "y", "z")]), params$clip_planes)
  keep[is.na(keep)] <- FALSE
  keep
}

# Places glyphs and returns a packet in model space.
#   data: one row per glyph, positions normalised to the panel box.
#   maps: 3 x 3 x n linear maps. Rows with space "data" map unit coordinates to
#     normalised scale space; rows with space "model" or "mm" map them to the
#     aspect-scaled model space the camera sees, in model units or millimetres.
#   unit: one unit mesh, or a list of unit meshes with `unit_index` giving
#     the mesh of each row.
# Glyphs stay rigid in curvilinear frames: data-space maps pass through the
# frame's Jacobian at the centre. Millimetre glyphs are resolved at draw time,
# when the device size is known; see resolve_glyph_packets_3d().
emit_glyph_instances_3d <- function(data, maps, space, unit, params, panel_params, unit_index = NULL, orientation = NULL) {
  units <- if (is.null(unit_index)) list(unit) else unit
  if (is.null(unit_index)) unit_index <- rep.int(1L, nrow(data))
  empty <- new_scene_packet(panel_id = panel_params$panel_id)
  frame <- frame_of_3d(panel_params)
  keep <- instance_rows_3d(data, maps, params, panel_params) & (!is.na(data$fill) | !is.na(data$colour))
  if (!any(keep)) return(empty)
  data <- data[keep, , drop = FALSE]; maps <- maps[, , keep, drop = FALSE]; space <- space[keep]
  if (!is.null(orientation)) orientation <- text_orientation_rows_3d(orientation, which(keep))
  unit_index <- unit_index[keep]
  n <- nrow(data)
  sizes <- vapply(units, function(u) nrow(u$vertices), integer(1))
  V <- sizes[unit_index]
  centre <- as.matrix(data[c("x", "y", "z")])
  model_centre <- frame$map(centre)
  scale <- panel_params$camera$aspect / max(panel_params$camera$aspect)
  data_rows <- which(space == "data")
  if (length(data_rows) && !isTRUE(frame$identity)) {
    jacobian <- frame_jacobian_3d(frame, centre[data_rows, , drop = FALSE])
    for (i in seq_along(data_rows)) maps[, , data_rows[i]] <- jacobian[, , i] %*% maps[, , data_rows[i]]
  }
  instance <- rep(seq_len(n), V)
  u <- do.call(rbind, lapply(units, `[[`, "vertices"))[sequence(V, from = cumsum(c(1L, head(sizes, -1L)))[unit_index]), , drop = FALSE]
  offset <- matrix(0, sum(V), 3L)
  for (k in 1:3) offset[, k] <- rowSums(u * t(maps[k, , instance]))
  rigid <- space[instance] != "data"
  millimetre <- space[instance] == "mm"
  mm_offset <- offset[millimetre, , drop = FALSE]
  offset[millimetre, ] <- mm_offset * glyph_placeholder_mm_3d
  offset[rigid, ] <- sweep(offset[rigid, , drop = FALSE], 2L, scale, "/")
  centres <- model_centre[instance, , drop = FALSE]
  rows <- data[instance, , drop = FALSE]
  xyz <- centres + offset
  rows$x <- xyz[, 1L]; rows$y <- xyz[, 2L]; rows$z <- xyz[, 3L]
  base <- cumsum(c(0L, head(V, -1L)))
  place <- function(m, bases) matrix(rep(as.vector(t(m)), length(bases)) + rep(bases, each = length(m)), ncol = ncol(m), byrow = TRUE)
  faces <- list(); edges <- matrix(integer(), 0L, 2L)
  for (group in split(seq_len(n), unit_index)) {
    mesh <- units[[unit_index[group[1L]]]]
    for (f in mesh$faces) {
      key <- as.character(ncol(f))
      faces[[key]] <- rbind(faces[[key]], place(f, base[group]))
    }
    edges <- rbind(edges, place(mesh$edges, base[group]))
  }
  faces <- unname(faces)
  packet <- packet_from_faces_edges_3d(rows, faces, edges, params, panel_params)
  scene_assert(length(packet$vertices$x) == nrow(rows), "glyph vertices must all be referenced")
  attr(packet, "model_space") <- TRUE
  if (any(millimetre)) attr(packet, "glyph_mm") <- list(rows = which(millimetre),
    centre = centres[millimetre, , drop = FALSE], offset = mm_offset)
  if (any(millimetre) && !is.null(orientation)) {
    glyph <- attr(packet, "glyph_mm")
    glyph$local <- u[millimetre, , drop = FALSE]
    glyph$orientation <- text_orientation_rows_3d(orientation, instance[millimetre])
    attr(packet, "glyph_mm") <- glyph
  }
  packet
}

# Aspect-scaled model units per millimetre before the device size is known.
glyph_placeholder_mm_3d <- 1 / 100

# Aspect-scaled model units per millimetre on screen, measured at the camera
# target, so a glyph at the centre of the view has its nominal size and the
# others keep their perspective scaling.
model_units_per_mm_3d <- function(camera, viewport) {
  basis <- frame_basis_3d(camera, viewport)
  scale <- camera$aspect / max(camera$aspect)
  step <- 1e-3
  points <- rbind(camera$target, camera$target + step * basis$basis[, 1L] / scale)
  packet <- new_scene_packet(vertices = list(x = points[, 1L], y = points[, 2L], z = points[, 3L]),
    corners = list(vertex = 1:2, source_row = 1:2),
    markers = list(offset = 1:2, footprint_id = c(1L, 1L), source_row = 1:2),
    footprints = list(kind = 1L, offset = 1L, count = 0L, radius_x_mm = 0, radius_y_mm = 0, angle = 0),
    styles = data.frame(style_id = 1L))
  camera$clip_data <- FALSE
  compiled <- scene_compile(packet, camera, viewport)
  f <- compiled$fragments
  at <- f$offset[match(1:2, f$source_row)]
  dx <- diff(compiled$vertices$x_npc[at]) * viewport$width_mm
  dy <- diff(compiled$vertices$y_npc[at]) * viewport$height_mm
  step / sqrt(dx^2 + dy^2)
}

# Millimetre glyphs take their size from the fitted camera and viewport.
resolve_glyph_packets_3d <- function(packets, camera, viewport) {
  deferred <- vapply(packets, function(p) !is.null(attr(p, "glyph_mm")), logical(1))
  quadric <- vapply(packets, function(p) !is.null(attr(p, "quadric_mm")), logical(1))
  if (!any(deferred | quadric)) return(packets)
  k <- model_units_per_mm_3d(camera, viewport)
  scale <- camera$aspect / max(camera$aspect)
  for (i in which(quadric)) packets[[i]] <- resolve_quadric_mm_3d(packets[[i]], k, scale)
  for (i in which(deferred)) {
    glyph <- attr(packets[[i]], "glyph_mm")
    offset <- glyph$offset
    if (!is.null(glyph$orientation)) {
      basis <- text_basis_3d(glyph$orientation, glyph$centre, camera, viewport)
      offset <- glyph$local[, 1L] * basis$u + glyph$local[, 2L] * basis$v
    }
    xyz <- glyph$centre + sweep(offset * k, 2L, scale, "/")
    for (k3 in 1:3) packets[[i]]$vertices[[k3]][glyph$rows] <- as.double(xyz[, k3])
  }
  packets
}
