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

emit_text_plane_3d <- function(data, params, panel_params) {
  plane <- params$plane %or3d% "xy"
  if (!(is.character(plane) && length(plane) == 1L && plane %in% text_planes_3d)) {
    stop("plane must be one of \"xy\", \"xz\" or \"yz\".", call. = FALSE)
  }
  if (isTRUE(params$parse)) stop("Text in a plane needs character labels; parse = TRUE is drawn on screen only.", call. = FALSE)
  require_text_outline_3d()
  rows <- which(valid_rows_3d(data, params) & !is.na(data$label))
  empty <- new_scene_packet(panel_id = panel_params$panel_id)
  if (!length(rows)) return(empty)
  data <- data[rows, , drop = FALSE]
  frame <- frame_of_3d(panel_params)
  camera <- panel_params$camera
  basis <- frame_basis_3d(camera, new_scene_viewport(100, 100))
  scale <- camera$aspect / max(camera$aspect)
  centre <- as.matrix(data[c("x", "y", "z")])
  axes <- text_plane_axes_3d(plane)
  u <- frame_directions_3d(frame, centre, axes$u, scale)
  v <- frame_directions_3d(frame, centre, axes$v, scale)
  n <- cross_rows_3d(u, v)
  # Seen from behind the plane, the reading direction turns round so that the
  # text is not mirrored.
  behind <- rowSums(n * toward_camera_3d(basis, camera, frame$map(centre))) < 0
  u[behind, ] <- -u[behind, ]; n[behind, ] <- -n[behind, ]
  angle <- (data$angle %or3d% rep(0, nrow(data))) * pi / 180
  reading <- cos(angle) * u + sin(angle) * v
  up <- -sin(angle) * u + cos(angle) * v
  fontsize <- data$size * text_size_pt_3d(params$size.unit)
  family <- data$family %or3d% rep("", nrow(data)); face <- data$fontface %or3d% rep(1, nrow(data))
  hjust <- data$hjust %or3d% rep(0.5, nrow(data)); vjust <- data$vjust %or3d% rep(0.5, nrow(data))
  key <- paste(data$label, fontsize, family, face, hjust, vjust, sep = "\r")
  distinct <- !duplicated(key)
  units <- lapply(which(distinct), function(i) text_mesh_3d(data$label[i], fontsize[i], family[i], face[i], hjust[i], vjust[i]))
  maps <- array(0, c(3L, 3L, nrow(data)))
  maps[, 1L, ] <- t(reading); maps[, 2L, ] <- t(up); maps[, 3L, ] <- t(n)
  data$fill <- data$colour
  data$colour <- NA_character_
  emit_glyph_instances_3d(data, maps, rep("mm", nrow(data)), units, params, panel_params,
    unit_index = match(key, key[distinct]))
}

register_geom_3d(GeomTextPlane3D, emit_text_plane_3d, parameters = list(plane = "xy"),
  positions = mark_positions_3d, tween = "row")
