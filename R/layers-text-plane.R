# Text lying in a coordinate plane of the data: each label is cut from its
# glyph outlines into faces that sit in the plane through its position, are
# foreshortened by the camera and depth-sort with the rest of the scene.

GeomTextPlane3D <- ggplot2::ggproto("GeomTextPlane3D", ggplot2::GeomText)

text_planes_3d <- c("xy", "xz", "yz")

# Reading direction and up direction of each plane in scale space.
text_plane_axes_3d <- function(plane) {
  spanned <- match(strsplit(plane, "", fixed = TRUE)[[1L]], c("x", "y", "z"))
  list(u = replace(numeric(3), spanned[1L], 1), v = replace(numeric(3), spanned[2L], 1))
}

# Unit vectors are scaled before squaring to accept finite extreme magnitudes.
text_direction_3d <- function(x, name) {
  if (!is.numeric(x) || length(x) != 3L || any(!is.finite(x)) || !any(x != 0)) {
    stop(name, " must be a finite nonzero numeric triple.", call. = FALSE)
  }
  x <- x / max(abs(x))
  x / sqrt(sum(x^2))
}

text_orientation_3d <- function(params, centre, panel_params) {
  supplied <- vapply(params[c("plane", "normal", "facing")], Negate(is.null), logical(1))
  if (sum(supplied) > 1L) stop("Supply only one of plane, normal or facing.", call. = FALSE)
  plane <- params$plane
  if (is.null(params$normal) && is.null(params$facing)) plane <- plane %or3d% "xy"
  if (!is.null(plane) && !(is.character(plane) && length(plane) == 1L && !is.na(plane) && plane %in% text_planes_3d)) {
    stop("plane must be one of \"xy\", \"xz\" or \"yz\".", call. = FALSE)
  }
  facing <- params$facing %or3d% "fixed"
  if (!is.null(params$facing) && !(is.character(facing) && length(facing) == 1L &&
      !is.na(facing) && facing %in% c("camera_plane", "camera_point"))) {
    stop("facing must be \"camera_plane\" or \"camera_point\".", call. = FALSE)
  }
  hint <- if (!is.null(params$up)) text_direction_3d(params$up, "up") else NULL
  frame <- frame_of_3d(panel_params)
  scale <- panel_params$camera$aspect / max(panel_params$camera$aspect)
  transform <- function(direction) {
    jacobian <- frame_jacobian_3d(frame, centre)
    unit_rows_3d(sweep(t(vapply(seq_len(nrow(centre)), function(i) drop(jacobian[, , i] %*% direction), numeric(3))), 2L, scale, "*"))
  }
  u <- v <- NULL
  if (!is.null(plane)) {
    axes <- text_plane_axes_3d(plane)
    u <- frame_directions_3d(frame, centre, axes$u, scale)
    v <- frame_directions_3d(frame, centre, axes$v, scale)
    if (!is.null(hint)) {
      n <- unit_rows_3d(cross_rows_3d(u, v))
      v <- text_up_3d(n, transform(hint))
      u <- unit_rows_3d(cross_rows_3d(v, n))
    }
  } else if (!is.null(params$normal)) {
    n <- matrix(text_direction_3d(params$normal, "normal"), 1L)
    default_up <- if (abs(n[1L, 3L]) > 1 - 1e-10) c(0, 1, 0) else c(0, 0, 1)
    v0 <- text_up_3d(n, matrix(hint %or3d% default_up, 1L))
    u0 <- cross_rows_3d(v0, n)
    v <- transform(v0[1L, ])
    n <- unit_rows_3d(cross_rows_3d(transform(u0[1L, ]), v))
    u <- unit_rows_3d(cross_rows_3d(v, n))
  }
  list(facing = facing, u = u, v = v,
    up = if (facing != "fixed" && !is.null(hint)) transform(hint) else NULL)
}

# The up hint is projected into the plane; a parallel hint uses the least
# aligned model axis so the basis remains defined at the poles.
text_up_3d <- function(normal, hint) {
  hint <- hint - rowSums(hint * normal) * normal
  parallel <- rowSums(hint^2) < 1e-20
  if (any(parallel)) {
    n <- normal[parallel, , drop = FALSE]
    axis <- diag(3)[max.col(-abs(n), ties.method = "first"), , drop = FALSE]
    hint[parallel, ] <- axis - rowSums(axis * n) * n
  }
  unit_rows_3d(hint)
}

# A text basis in aspect-scaled model space, resolved for the drawing camera.
text_basis_3d <- function(spec, centre, camera, viewport) {
  basis <- frame_basis_3d(camera, viewport)
  toward <- toward_camera_3d(basis, camera, centre)
  if (spec$facing == "fixed") {
    u <- spec$u; v <- spec$v
    n <- cross_rows_3d(u, v)
    behind <- rowSums(n * toward) < 0
    u[behind, ] <- -u[behind, ]; n[behind, ] <- -n[behind, ]
  } else {
    n <- unit_rows_3d(if (spec$facing == "camera_point") toward else
      matrix(if (basis$perspective) -basis$forward else -basis$ray, nrow(centre), 3L, byrow = TRUE))
    hint <- spec$up %or3d% matrix(basis$basis[, 2L], nrow(centre), 3L, byrow = TRUE)
    v <- text_up_3d(n, hint)
    u <- unit_rows_3d(cross_rows_3d(v, n))
  }
  angle <- spec$angle * pi / 180
  list(u = cos(angle) * u + sin(angle) * v, v = -sin(angle) * u + cos(angle) * v, n = n)
}

text_orientation_rows_3d <- function(spec, rows) {
  lapply(spec, function(x) if (is.matrix(x)) x[rows, , drop = FALSE] else if (is.numeric(x)) x[rows] else x)
}

emit_text_plane_3d <- function(data, params, panel_params) {
  centre <- as.matrix(data[c("x", "y", "z")])
  spec <- text_orientation_3d(params, centre, panel_params)
  if (isTRUE(params$parse)) stop("Text in a plane needs character labels; parse = TRUE is drawn on screen only.", call. = FALSE)
  require_text_outline_3d()
  rows <- which(valid_rows_3d(data, params) & !is.na(data$label))
  empty <- new_scene_packet(panel_id = panel_params$panel_id)
  if (!length(rows)) return(empty)
  data <- data[rows, , drop = FALSE]
  spec <- text_orientation_rows_3d(spec, rows)
  frame <- frame_of_3d(panel_params)
  camera <- panel_params$camera
  centre <- as.matrix(data[c("x", "y", "z")])
  spec$angle <- data$angle %or3d% rep(0, nrow(data))
  basis <- text_basis_3d(spec, frame$map(centre), camera, new_scene_viewport(100, 100))
  fontsize <- data$size * text_size_pt_3d(params$size.unit)
  family <- data$family %or3d% rep("", nrow(data)); face <- data$fontface %or3d% rep(1, nrow(data))
  hjust <- data$hjust %or3d% rep(0.5, nrow(data)); vjust <- data$vjust %or3d% rep(0.5, nrow(data))
  key <- paste(data$label, fontsize, family, face, hjust, vjust, sep = "\r")
  distinct <- !duplicated(key)
  units <- lapply(which(distinct), function(i) text_mesh_3d(data$label[i], fontsize[i], family[i], face[i], hjust[i], vjust[i]))
  maps <- array(0, c(3L, 3L, nrow(data)))
  maps[, 1L, ] <- t(basis$u); maps[, 2L, ] <- t(basis$v); maps[, 3L, ] <- t(basis$n)
  data$fill <- data$colour
  data$colour <- NA_character_
  emit_glyph_instances_3d(data, maps, rep("mm", nrow(data)), units, params, panel_params,
    unit_index = match(key, key[distinct]), orientation = spec)
}

register_geom_3d(GeomTextPlane3D, emit_text_plane_3d, parameters = list(plane = NULL, normal = NULL, up = NULL, facing = NULL),
  positions = mark_positions_3d, tween = "row")
