# Embedded textures retain a print-resolution sample grid on vector devices.
texture_image_dpi_3d <- function(viewport) max(viewport$dpi, 300)

# Translucent runs sample at device centres so disjoint depth-order fragments
# retain disjoint coverage when the device composites their images.
texture_run_sampling_3d <- function(packet, viewport) {
  opaque <- all(packet$faces$opacity == 1) && all(packet$faces$coverage_opaque %or3d% TRUE)
  if (opaque) viewport$dpi <- texture_image_dpi_3d(viewport)
  list(viewport = viewport, antialias = if (opaque) 4L else 1L, interpolate = opaque)
}

# The source faces of a compiled run, retaining corner attributes and UVs.
texture_run_packet_3d <- function(scene, rows) {
  packet <- scene$source
  packet$styles <- scene$styles
  packet$materials <- scene$materials
  fragments <- scene$fragments
  selected <- unique(fragments$source_index[rows])
  packet$faces <- lapply(packet$faces, `[`, selected)
  attr(packet, "face_clip") <- lapply(selected, function(source) {
    lapply(rows[fragments$source_index[rows] == source], function(row) {
      index <- seq.int(fragments$offset[row], length.out = fragments$count[row])
      cbind(scene$vertices$x_npc[index], scene$vertices$y_npc[index])
    })
  })
  for (kind in c("polylines", "markers", "text", "quadrics"))
    packet[[kind]] <- lapply(packet[[kind]], `[`, integer())
  packet$styles$colour <- NA_character_
  class(packet) <- "ggplot3d_scene_packet"
  packet
}

# Each run shares one screen-space image with perspective-correct texture
# coordinates, linear-light shading and its source fragments' coverage.
texture_lighting_grob_3d <- function(scene, rows, viewport, light, name) {
  packet <- texture_run_packet_3d(scene, rows)
  sampling <- texture_run_sampling_3d(packet, viewport)
  camera <- scene$camera_input
  result <- raster_scene_3d(packet, camera, sampling$viewport, sampling$antialias, light, blend = "srgb")
  image <- raster_image_grob_3d(result)
  image$interpolate <- sampling$interpolate
  grid::gTree(children = grid::gList(image), name = name, texture_lighting = "sampled")
}
