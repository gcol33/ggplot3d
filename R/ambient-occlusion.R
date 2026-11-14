utils::globalVariables(c("ambient_occlusion_prepare_cpp", "ambient_occlusion_cpp", "raster_ambient_cpp"))

# One mesh BVH shared by a panel's vector and raster passes. Faces retain
# their opacity as transmission; furniture, strokes and text are excluded.
ambient_light_3d <- function(packet, camera, viewport, light) {
  spec <- light$ao
  if (is.null(light)) return(light)
  if (is.null(spec)) {
    light$occlusion <- NULL
    return(light)
  }
  if (!is.null(light$occlusion)) return(light)
  resolved <- scene_compile(new_scene_packet(), camera, viewport)$camera
  packet <- scene_finite_packet_3d(packet, camera$pre %or3d% diag(4))
  if (isTRUE(camera$clip_data)) packet <- clip_packet_planes_3d(packet, unit_box_planes_3d())
  tab <- packet$faces
  if (!length(tab$offset)) return(light)
  styles <- packet$styles[match(tab$style_id, packet$styles$style_id), , drop = FALSE]
  fill <- style_value_3d(styles, "fill", NA_character_)
  fill_alpha <- as.double(colour_channels_3d(fill, alpha = TRUE)[4L, ]) / 255
  fill_alpha[is.na(fill)] <- 0
  alpha <- fill_alpha * tab$opacity
  textures <- panel_textures_3d(styles)
  textured <- textures$index > 0L
  alpha[textured] <- tab$opacity[textured]
  keep <- which(tab$layer_id > 0L & tab$partition == 1L & alpha > 0 & tab$count >= 3L)
  if (!length(keep)) return(light)
  n <- tab$count[keep] - 2L
  face <- rep(keep, n); step <- sequence(n)
  corner <- rbind(tab$offset[face], tab$offset[face] + step, tab$offset[face] + step + 1L)
  triangles <- matrix(packet$corners$vertex[corner], 3L)
  position <- sweep(scene_vertices_xyz_3d(packet$vertices), 2L, resolved$model_scale, "*")
  vertex <- as.vector(triangles)
  colour <- cbind(matrix(0, length(vertex), 3L), rep(alpha[face], each = 3L))
  mesh <- list(position = position[vertex, , drop = FALSE], normal = matrix(0, length(vertex), 3L),
    colour = colour, triangles = matrix(seq_along(vertex), 3L))
  if (any(textured) && all(c("u", "v") %in% names(packet$corners))) {
    mesh$textures <- lapply(textures$textures, `[[`, "image")
    mesh$interpolate <- vapply(textures$textures, `[[`, logical(1), "interpolate")
    mesh$texture <- as.integer(textures$index[face])
    mesh$fill_alpha <- fill_alpha[face]
    mesh$uv <- as.matrix(as.data.frame(packet$corners)[as.vector(corner), c("u", "v")])
  }
  light$occlusion <- ambient_occlusion_prepare_cpp(mesh, spec$samples, spec$radius, spec$amount,
    1e-6 * max(resolved$model_scale))
  light
}
