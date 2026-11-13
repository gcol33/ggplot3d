require_geometry_package_3d <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Install the '", package, "' package for this geometry input.", call. = FALSE)
}

#' Fortify an indexed three-dimensional mesh
#' @param vertices Data frame containing x, y, z and optional .vertex and .mesh keys.
#' @param faces Matrix with one triangle or quad per row, or a data frame with
#'   .face, .corner, .vertex and optional .mesh keys. Indices are one-based.
#' @param vertex_data,face_data Attribute tables joined on .vertex or .face,
#'   together with .mesh when supplied. Keys must be unique.
#' @return A keyed corner data frame of class ggplot3d_mesh.
#' @examples
#' v <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 0, 1))
#' m <- mesh_3d(v, matrix(1:3, nrow = 1))
#' ggplot2::ggplot() + geom_mesh_3d(data = m) + coord_3d()
#' @export
mesh_3d <- function(vertices, faces, vertex_data = NULL, face_data = NULL) {
  vertices <- as.data.frame(vertices)
  if (!all(c("x", "y", "z") %in% names(vertices)) && ncol(vertices) == 3L) names(vertices) <- c("x", "y", "z")
  if (!all(c("x", "y", "z") %in% names(vertices))) stop("Vertices require x, y and z columns.", call. = FALSE)
  if (is.null(vertices$.vertex)) vertices$.vertex <- seq_len(nrow(vertices))
  if (is.null(vertices$.mesh)) vertices$.mesh <- 1L
  if (is.matrix(faces)) {
    if (!ncol(faces) %in% c(3L, 4L)) stop("Mesh faces must be triangles or quads.", call. = FALSE)
    faces <- data.frame(.face = rep(seq_len(nrow(faces)), each = ncol(faces)),
      .corner = rep(seq_len(ncol(faces)), nrow(faces)), .vertex = as.vector(t(faces)), .mesh = 1L)
  }
  if (!all(c(".face", ".corner", ".vertex") %in% names(faces))) stop("Faces require .face, .corner and .vertex keys.", call. = FALSE)
  if (is.null(faces$.mesh)) faces$.mesh <- 1L
  for (d in list(vertices[c(".mesh", ".vertex")], faces[c(".mesh", ".face", ".corner", ".vertex")])) {
    if (any(!vapply(d, is.numeric, logical(1))) || any(!is.finite(as.matrix(d))) || any(as.matrix(d) < 1 | as.matrix(d) > .Machine$integer.max | as.matrix(d) != trunc(as.matrix(d)))) {
      stop("Mesh keys must be positive finite integers.", call. = FALSE)
    }
  }
  if (anyDuplicated(vertices[c(".mesh", ".vertex")]) || anyDuplicated(faces[c(".mesh", ".face", ".corner")])) stop("Mesh keys must be unique.", call. = FALSE)
  key <- function(d, id) paste(as.integer(d$.mesh), as.integer(d[[id]]), sep = ":")
  index <- match(key(faces, ".vertex"), key(vertices, ".vertex"))
  if (anyNA(index)) stop("Face indices reference missing vertices.", call. = FALSE)
  result <- cbind(faces, vertices[index, setdiff(names(vertices), c(".mesh", ".vertex")), drop = FALSE])
  for (kind in c("vertex", "face")) {
    attrs <- if (kind == "vertex") vertex_data else face_data
    if (is.null(attrs)) next
    id <- paste0(".", kind)
    if (!id %in% names(attrs)) stop("Attribute tables require ", id, " keys.", call. = FALSE)
    keys <- intersect(c(".mesh", id), names(attrs))
    if (anyDuplicated(attrs[keys])) stop("Attribute keys must be unique.", call. = FALSE)
    left <- if (".mesh" %in% keys) key(result, id) else result[[id]]
    right <- if (".mesh" %in% keys) key(attrs, id) else attrs[[id]]
    matched <- match(left, right)
    if (anyNA(matched)) stop("Attribute table has missing mesh keys.", call. = FALSE)
    for (field in setdiff(names(attrs), keys)) {
      if (field %in% names(result)) stop("Duplicate mesh attribute: ", field, ".", call. = FALSE)
      result[[field]] <- attrs[[field]][matched]
    }
  }
  rownames(result) <- NULL
  class(result) <- c("ggplot3d_mesh", "data.frame")
  result
}

#' @importFrom ggplot2 fortify
#' @examples
#' v <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 0, 1))
#' fortify(mesh_3d(v, matrix(1:3, nrow = 1)))
#' @export
ggplot2::fortify

#' @export
fortify.ggplot3d_mesh <- function(model, data = NULL, ...) as.data.frame(model)

#' @export
fortify.mesh3d <- function(model, data = NULL, ...) {
  require_geometry_package_3d("rgl")
  if (length(model$material$texture) && any(is.na(model$material$texture) | nzchar(model$material$texture))) stop("Mesh textures are unsupported materials.", call. = FALSE)
  if (length(model$ip) || length(model$is)) stop("Mesh point and segment components are unsupported; supply triangle/quad faces.", call. = FALSE)
  vb <- model$vb
  if (!is.matrix(vb) || nrow(vb) != 4L || any(!is.finite(vb)) || any(vb[4L, ] == 0)) stop("Mesh homogeneous coordinates require finite nonzero w.", call. = FALSE)
  vertices <- as.data.frame(t(sweep(vb[1:3, , drop = FALSE], 2L, vb[4L, ], `/`)))
  names(vertices) <- c("x", "y", "z")
  tables <- list(); offset <- 0L
  for (indices in list(model$it, model$ib)) {
    if (is.null(indices) || !length(indices)) next
    tables[[length(tables) + 1L]] <- data.frame(.face = rep(seq_len(ncol(indices)) + offset, each = nrow(indices)),
      .corner = rep(seq_len(nrow(indices)), ncol(indices)), .vertex = as.vector(indices))
    offset <- offset + ncol(indices)
  }
  if (!length(tables)) stop("Mesh has no triangle or quad faces.", call. = FALSE)
  result <- mesh_3d(vertices, do.call(rbind, tables))
  for (field in setdiff(names(model$material), "texture")) {
    value <- model$material[[field]]
    if (!length(value) || !is.atomic(value)) next
    if (!length(value) %in% c(1L, nrow(vertices), offset)) stop("Mesh materials require scalar, vertex or face length.", call. = FALSE)
    name <- paste0("material_", if (field == "color") "colour" else field)
    result[[name]] <- if (length(value) == nrow(vertices)) value[result$.vertex] else rep_len(value, offset)[result$.face]
  }
  result
}

spatial_corners_3d <- function(data, extrusion = FALSE) {
  if (!inherits(data, "sf") && !inherits(data, "sfc")) return(data)
  require_geometry_package_3d("sf")
  if (inherits(data, "sfc")) data <- sf::st_sf(geometry = data)
  geometry <- sf::st_geometry(data)
  validity_geometry <- lapply(geometry, function(g) {
    if (class(g)[2L] %in% c("TIN", "POLYHEDRALSURFACE")) sf::st_geometrycollection(lapply(g, sf::st_polygon)) else g
  })
  valid <- sf::st_is_valid(sf::st_sfc(validity_geometry, crs = sf::st_crs(data)))
  if (anyNA(valid) || any(!valid)) stop("Spatial geometry contains invalid features.", call. = FALSE)
  attrs <- sf::st_drop_geometry(data)
  chunks <- list(); face <- 0L; vertex <- 0L
  for (feature in seq_along(geometry)) {
    g <- geometry[[feature]]
    if (length(g) == 0L) next
    type <- class(g)[2L]
    allowed <- if (extrusion) c("POLYGON", "MULTIPOLYGON") else c("POLYGON", "MULTIPOLYGON", "TRIANGLE", "TIN", "POLYHEDRALSURFACE")
    if (!type %in% allowed) stop("Unsupported spatial geometry: ", type, ".", call. = FALSE)
    parts <- if (type %in% c("POLYGON", "TRIANGLE")) list(g) else g
    for (part in seq_along(parts)) {
      face <- face + 1L
      corner <- 0L
      for (ring in seq_along(parts[[part]])) {
        xyz <- parts[[part]][[ring]]
        if (nrow(xyz) > 1L && all(xyz[1L, ] == xyz[nrow(xyz), ])) xyz <- xyz[-nrow(xyz), , drop = FALSE]
        if (!extrusion && ncol(xyz) < 3L) stop("Spatial meshes require POLYGON Z or TIN Z coordinates.", call. = FALSE)
        n <- nrow(xyz)
        chunk <- attrs[rep(feature, n), , drop = FALSE]
        chunk$.x3d <- xyz[, 1L]; chunk$.y3d <- xyz[, 2L]
        chunk$.z3d <- if (extrusion) 0 else xyz[, 3L]
        chunk$.feature <- feature; chunk$.part <- part; chunk$.ring <- ring
        chunk$.mesh <- feature; chunk$.face <- face
        chunk$.vertex <- seq_len(n) + vertex; chunk$.corner <- seq_len(n) + corner
        vertex <- vertex + n
        corner <- corner + n
        chunks[[length(chunks) + 1L]] <- chunk
      }
    }
  }
  if (!length(chunks)) stop("Spatial geometry has no polygon vertices.", call. = FALSE)
  result <- do.call(rbind, chunks)
  attr(result, "geometry_crs_3d") <- sf::st_crs(data)
  attr(result, "geometry_geographic_3d") <- isTRUE(sf::st_is_longlat(data))
  attr(result, "geometry_extrusion_3d") <- extrusion
  result
}

prepare_spatial_aesthetics_3d <- function(out, data, plot, params) {
  if (".x3d" %in% names(data)) {
    target <- plot@coordinates$config$crs
    source <- attr(data, "geometry_crs_3d")
    xy <- data[c(".x3d", ".y3d")]
    if (!is.null(target)) {
      require_geometry_package_3d("sf")
      if (is.null(source) || is.na(source)) stop("A target CRS requires a declared source CRS.", call. = FALSE)
      projected <- sf::st_transform(sf::st_as_sf(xy, coords = names(xy), crs = source), target)
      if (isTRUE(sf::st_is_longlat(projected))) stop("Planar geometry requires a projected target CRS.", call. = FALSE)
      xy[] <- as.data.frame(sf::st_coordinates(projected))
    } else if (isTRUE(attr(data, "geometry_geographic_3d"))) stop("Geographic geometry requires coord_3d(crs = <projected CRS>).", call. = FALSE)
    out$x <- xy[[1L]]; out$y <- xy[[2L]]
    if (!isTRUE(attr(data, "geometry_extrusion_3d"))) out$z <- data$.z3d
    out$subgroup <- data$.ring
  }
  for (key in intersect(c(".mesh", ".face", ".vertex", ".corner", ".feature", ".part"), names(data))) out[[key]] <- data[[key]]
  if (".feature" %in% names(data)) {
    out$.origin <- data$.feature
    out$group <- as.integer(interaction(out$group, data$.feature, data$.part, drop = TRUE))
  } else if (".vertex" %in% names(data)) out$.origin <- data$.vertex
  out
}

prepare_mesh_data_3d <- function(data) {
  if (inherits(data, "mesh3d")) data <- ggplot2::fortify(data)
  data <- spatial_corners_3d(data)
  if (!all(c(".face", ".vertex", ".corner") %in% names(data))) stop("Mesh input requires keyed corners; use mesh_3d(vertices, faces).", call. = FALSE)
  if (is.null(data$.mesh)) data$.mesh <- 1L
  data
}

prepare_mesh_aesthetics_3d <- function(out, data, plot, params) {
  out <- prepare_spatial_aesthetics_3d(out, data, plot, params)
  for (axis in c("x", "y", "z")) if (is.null(out[[axis]])) out[[axis]] <- data[[axis]]
  out
}

StatMesh3D <- ggplot2::ggproto("StatMesh3D", ggplot2::Stat,
  required_aes = c("x", "y", "z", ".mesh", ".face", ".corner", ".vertex"),
  compute_panel = function(data, scales, subdivide = 0L, remesh = NULL, decimate = NULL, smooth = 0L,
                           curvature = FALSE, crease = NULL) {
    mesh_face_materials_3d(mesh_pipeline_3d(data, subdivide, remesh, decimate, smooth, curvature, crease))
  }
)

# Numeric fill averaged over the corners of each face; other materials must
# already be constant per face.
mesh_face_materials_3d <- function(data) {
  rows <- order(data$.mesh, data$.face, data$.corner)
  ids <- mesh_face_ids_3d(data, rows)
  for (field in intersect(c("fill", "colour", "alpha", "linewidth", "linetype"), names(data))) {
    value <- data[[field]]
    if (field == "fill" && is.numeric(value)) {
      data[[field]] <- mesh_face_mean_3d(value, ids)
    } else {
      keys <- match(value, unique(value))
      adjacent <- head(rows, -1L); next_row <- tail(rows, -1L)
      if (any(ids[adjacent] == ids[next_row] & keys[adjacent] != keys[next_row])) stop("Mesh materials must be constant per face; numeric vertex fill is averaged before scaling.", call. = FALSE)
    }
  }
  data
}

# Face number of every corner row, counting faces in the order of rows.
mesh_face_ids_3d <- function(data, rows = order(data$.mesh, data$.face, data$.corner)) {
  first <- c(TRUE, diff(data$.mesh[rows]) != 0L | diff(data$.face[rows]) != 0L)
  ids <- integer(nrow(data)); ids[rows] <- cumsum(first)
  ids
}

mesh_face_mean_3d <- function(value, ids) {
  sums <- rowsum(value, ids, reorder = TRUE)
  as.double(sums[, 1L] / tabulate(ids))[ids]
}

# Mesh verbs applied in a fixed order: refine, resample, coarsen, smooth,
# then annotate. A rebuilt mesh no longer maps to input rows.
mesh_pipeline_3d <- function(data, subdivide, remesh, decimate, smooth, curvature, crease) {
  subdivide <- mesh_count_3d(subdivide, "subdivide"); smooth <- mesh_count_3d(smooth, "smooth")
  remesh <- if (isFALSE(remesh)) NULL else remesh
  rebuilt <- subdivide > 0L || !is.null(remesh) || !is.null(decimate) || smooth > 0L
  if (subdivide > 0L) data <- mesh_subdivide(data, subdivide)
  if (isTRUE(remesh)) data <- mesh_remesh(data)
  else if (!is.null(remesh)) data <- mesh_remesh(data, remesh)
  if (!is.null(decimate)) data <- mesh_decimate(data, decimate)
  if (smooth > 0L) data <- mesh_smooth(data, smooth)
  if (isTRUE(curvature)) data <- mesh_curvature(data)
  if (!is.null(crease)) data <- mesh_normals(data, crease)
  if (rebuilt) data$.source <- NULL
  data
}

GeomMesh3D <- ggplot2::ggproto("GeomMesh3D", ggplot2::Geom,
  required_aes = c("x", "y", "z", ".mesh", ".face", ".vertex", ".corner"),
  optional_aes = c("subgroup", ".origin", ".feature", ".part"),
  default_aes = solid_defaults_3d(), draw_key = ggplot2::draw_key_polygon
)

#' Draw an external triangle or polygon mesh
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param vertices,faces Optional vertex data frame and face index matrix, passed to mesh_3d().
#'   With faces supplied, data may hold the vertices.
#' @param shade Whether to apply lighting.
#' @param wireframe Draw original mesh edges with shared edges deduplicated.
#' @param hidden Hidden stroke policy.
#' @param subdivide,smooth Rounds of [mesh_subdivide()] and iterations of
#'   [mesh_smooth()].
#' @param remesh Target edge length for [mesh_remesh()], or TRUE for the mean
#'   edge length.
#' @param decimate Target face count or fraction for [mesh_decimate()].
#' @param curvature Whether to add the computed variables of
#'   [mesh_curvature()], for use with `after_stat()`.
#' @param crease Crease angle for [mesh_normals()]; NULL keeps the input's
#'   normals.
#' @param outline Feature lines from [outline_3d()], or NULL for none.
#' @details Data accepts mesh_3d() output, rgl mesh3d objects and projected sf
#'   POLYGON Z or TIN Z objects. Numeric vertex fill is averaged per face before
#'   colour scaling. Other materials must be constant within each face.
#'
#'   The mesh parameters run the [mesh processing][mesh_processing] verbs on
#'   each mesh of a panel, in transformed position units, in the order
#'   subdivide, remesh, decimate, smooth, curvature, crease.
#' @return A ggplot2 layer.
#' @examples
#' v <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0, 0, 1, 1))
#' ggplot2::ggplot() + geom_mesh_3d(vertices = v, faces = matrix(1:4, 1)) + coord_3d()
#' cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
#' quads <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5), c(3, 7, 8, 4),
#'   c(1, 5, 7, 3), c(2, 4, 8, 6))
#' ggplot2::ggplot() +
#'   geom_mesh_3d(ggplot2::aes(fill = ggplot2::after_stat(mean_curvature)),
#'     vertices = cube, faces = quads, subdivide = 3, curvature = TRUE) +
#'   coord_3d()
#' @export
geom_mesh_3d <- function(mapping = NULL, data = NULL, stat = StatMesh3D, position = "identity", ...,
                         vertices = NULL, faces = NULL, shade = TRUE, wireframe = FALSE, hidden = "hide",
                         subdivide = 0L, remesh = NULL, decimate = NULL, smooth = 0L, curvature = FALSE, crease = NULL,
                         outline = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!is.null(vertices) || !is.null(faces)) {
    if (!is.null(data) && !is.null(vertices)) stop("Supply data or vertices/faces.", call. = FALSE)
    data <- mesh_3d(vertices %or3d% data, faces)
  }
  if (inherits(data, "mesh3d")) data <- ggplot2::fortify(data)
  make_layer_3d(GeomMesh3D, mapping, data, stat, position,
    c(list(shade = shade, wireframe = wireframe, hidden = hidden, subdivide = subdivide, remesh = remesh,
      decimate = decimate, smooth = smooth, curvature = curvature, crease = crease, outline = outline, na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}

emit_mesh_3d <- function(data, params, panel_params) {
  valid <- valid_rows_3d(data, params)
  rows <- order(data$.mesh, data$.face, data$.corner)
  first <- c(TRUE, diff(data$.mesh[rows]) != 0L | diff(data$.face[rows]) != 0L)
  counts <- diff(c(which(first), length(rows) + 1L))
  if (all(counts == 3L) && !isTRUE(params$wireframe) && all(is.na(data$colour)) && is.null(data$subgroup)) {
    faces <- matrix(rows, ncol = 3L, byrow = TRUE)
    corners <- matrix(data$.corner[faces], ncol = 3L)
    vertices <- matrix(data$.vertex[faces], ncol = 3L)
    if (any(corners[, 1L] == corners[, 2L] | corners[, 2L] == corners[, 3L] | corners[, 1L] == corners[, 3L]) ||
        any(vertices[, 1L] == vertices[, 2L] | vertices[, 2L] == vertices[, 3L] | vertices[, 1L] == vertices[, 3L])) stop("Mesh face corners and vertices must be unique.", call. = FALSE)
    keep <- rowSums(matrix(valid[faces], ncol = 3L)) == 3L & !is.na(data$fill[faces[, 1L]])
    faces <- faces[keep, , drop = FALSE]
    packet <- packet_from_primitives_3d(data, as.vector(t(faces)), rep("faces", nrow(faces)), params, panel_params$panel_id, rep.int(3L, nrow(faces)))
    return(outline_merge_3d(packet, data, faces, params, panel_params))
  }
  groups <- split(rows, rep(seq_along(counts), counts))
  groups <- Filter(function(r) all(valid[r]), groups)
  if (!length(groups)) return(new_scene_packet(panel_id = panel_params$panel_id))
  triangles <- edges <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    r <- groups[[i]]
    if (anyDuplicated(data$.corner[r]) || anyDuplicated(data$.vertex[r])) stop("Mesh face corners and vertices must be unique.", call. = FALSE)
    rings <- split(seq_along(r), data$subgroup[r] %or3d% rep(1L, length(r)))
    triangles[[i]] <- mesh_face_triangles_3d(data, r, rings)
    edges[[i]] <- do.call(rbind, lapply(rings, function(ids) cbind(r[ids], r[c(ids[-1L], ids[1L])])))
  }
  e <- unique_edges_3d(do.call(rbind, edges), data$.vertex, data$.mesh)
  t <- do.call(rbind, triangles)
  outline_merge_3d(packet_from_faces_edges_3d(data, t, e, params, panel_params), data, t, params, panel_params)
}

# Triangles (rows of data) of the polygon face on rows r with rings given as
# positions in r, wound like the first ring.
mesh_face_triangles_3d <- function(data, r, rings) {
  if (length(r) == 3L && length(rings) == 1L) return(matrix(r, nrow = 1L))
  topology <- triangulate_polygon_3d(data[r, , drop = FALSE], rings)
  t <- matrix(r[unlist(topology$triangles)], ncol = 3L, byrow = TRUE)
  normal <- polygon_normal_3d(as.matrix(data[r[rings[[1L]]], c("x", "y", "z")]))
  a <- as.numeric(data[t[1L, 2L], c("x", "y", "z")] - data[t[1L, 1L], c("x", "y", "z")])
  b <- as.numeric(data[t[1L, 3L], c("x", "y", "z")] - data[t[1L, 1L], c("x", "y", "z")])
  cross <- c(a[2] * b[3] - a[3] * b[2], a[3] * b[1] - a[1] * b[3], a[1] * b[2] - a[2] * b[1])
  if (sum(cross * normal) < 0) t <- t[, c(1L, 3L, 2L), drop = FALSE]
  t
}

register_geom_3d(GeomMesh3D, emit_mesh_3d, parameters = list(outline = NULL),
  prepare_data = prepare_mesh_data_3d, prepare_aesthetics = prepare_mesh_aesthetics_3d)

# Mesh stats that add intrinsic quantities to the corner table, averaged
# over each face so that a fill mapped from them is constant per face.
mesh_face_columns_3d <- function(data, columns) {
  ids <- mesh_face_ids_3d(data)
  for (name in columns) data[[name]] <- mesh_face_mean_3d(data[[name]], ids)
  data
}

StatGeodesicDistance3D <- ggplot2::ggproto("StatGeodesicDistance3D", StatMesh3D,
  compute_panel = function(self, data, scales, source, time = 1, boundary = "average") {
    data <- ggplot2::ggproto_parent(StatMesh3D, self)$compute_panel(data, scales)
    mesh_face_columns_3d(mesh_geodesic_distance(data, source, time, boundary), "distance")
  }
)

StatLogmap3D <- ggplot2::ggproto("StatLogmap3D", StatMesh3D,
  compute_panel = function(self, data, scales, source, direction = NULL, time = 1) {
    data <- ggplot2::ggproto_parent(StatMesh3D, self)$compute_panel(data, scales)
    mesh_face_columns_3d(mesh_logmap(data, source, direction, time), c("logmap_u", "logmap_v"))
  }
)

StatGeodesicPath3D <- ggplot2::ggproto("StatGeodesicPath3D", StatMesh3D,
  compute_panel = function(data, scales, from, to, max_iterations = 100000L) {
    path <- mesh_geodesic_path(data, from, to, max_iterations)
    path$length <- attr(path, "length")[path$path]
    path$group <- path$path
    attr(path, "length") <- NULL
    path
  }
)

# A path geom that accepts mesh corner tables as input, so a mesh stat can
# turn them into polylines.
GeomGeodesicPath3D <- ggplot2::ggproto("GeomGeodesicPath3D", ggplot2::GeomPath)

register_geom_3d(GeomGeodesicPath3D, emit_paths_3d,
  prepare_data = prepare_mesh_data_3d, prepare_aesthetics = prepare_mesh_aesthetics_3d)

#' Geodesic distance, log map and shortest paths on a mesh
#'
#' Stats over a mesh layer's data (a [mesh_3d()] corner table, an rgl
#' `mesh3d`, or the corners a surface stat returns) that measure along the
#' surface. They run the [intrinsic geometry][mesh_geodesic] verbs on each
#' panel, in transformed position units, so points are given in the same
#' units as the positions.
#'
#' `stat_geodesic_distance_3d()` computes `distance`, the geodesic distance
#' from the nearest of the `source` points, by the heat method.
#' `stat_logmap_3d()` computes `logmap_u` and `logmap_v`, polar coordinates
#' around `source` whose radius is the geodesic distance and whose angle is
#' that of the shortest geodesic, by the vector heat method; mapped to a
#' periodic fill they place a texture or decal on the surface. Both draw the
#' mesh, and their computed variables are averaged over each face, so a fill
#' mapped from them is constant per face; the verbs give the values at the
#' vertices.
#'
#' `stat_geodesic_path_3d()` draws the shortest paths between the mesh
#' vertices closest to the rows of `from` and `to`, found by FlipOut, as
#' polylines lying on the surface. It computes `s`, the arc length along each
#' path, and `length`, the length of the whole path.
#' @inheritParams geom_mesh_3d
#' @inheritParams mesh_geodesic
#' @param geom Geom drawing the result.
#' @return A ggplot2 layer.
#' @examples
#' sphere <- function(u, v) list(x = cos(u) * sin(v), y = sin(u) * sin(v), z = cos(v))
#' m <- ggplot2::layer_data(ggplot2::ggplot() +
#'   stat_parametric_3d(fun = sphere, v = c(0, pi), n = c(40, 21), closed = c(TRUE, FALSE)) +
#'   coord_3d())
#' m <- mesh_3d(unique(m[c("x", "y", "z", ".vertex")]),
#'   data.frame(.face = m$.face, .corner = m$.corner, .vertex = m$.vertex))
#' ggplot2::ggplot(m) +
#'   stat_geodesic_distance_3d(ggplot2::aes(fill = ggplot2::after_stat(distance)),
#'     source = c(0, 0, 1)) +
#'   stat_geodesic_path_3d(from = c(1, 0, 0), to = c(0, 0.6, 0.8), colour = "white") +
#'   coord_3d()
#' ggplot2::ggplot(m) +
#'   stat_logmap_3d(ggplot2::aes(fill = ggplot2::after_stat(
#'     (floor(4 * logmap_u) + floor(4 * logmap_v)) %% 2)),
#'     source = c(0, 0, 1), direction = c(1, 0, 0)) +
#'   coord_3d()
#' @export
stat_geodesic_distance_3d <- function(mapping = NULL, data = NULL, geom = GeomMesh3D, position = "identity", ...,
                                      source, time = 1, boundary = c("average", "neumann", "dirichlet"),
                                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatGeodesicDistance3D, position,
    c(list(source = mesh_points_3d(source, "source"), time = time, boundary = match.arg(boundary), na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}

#' @rdname stat_geodesic_distance_3d
#' @export
stat_logmap_3d <- function(mapping = NULL, data = NULL, geom = GeomMesh3D, position = "identity", ...,
                           source, direction = NULL, time = 1, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatLogmap3D, position,
    c(list(source = mesh_points_3d(source, "source"), direction = direction, time = time, na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}

#' @rdname stat_geodesic_distance_3d
#' @export
stat_geodesic_path_3d <- function(mapping = NULL, data = NULL, geom = GeomGeodesicPath3D, position = "identity", ...,
                                  from, to, max_iterations = 100000L, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatGeodesicPath3D, position,
    c(list(from = mesh_points_3d(from, "from"), to = mesh_points_3d(to, "to"), max_iterations = max_iterations,
      na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

