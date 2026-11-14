# R devices composite encoded sRGB. Per-sample factors compensate for that
# transfer curve, using the shared raster sampler and lighting kernel.
texture_lighting_device_ok_3d <- function(caps = grDevices::dev.capabilities()) {
  getRversion() >= "4.2.0" && "alpha" %in% caps$masks &&
    all(c("multiply", "screen") %in% caps$compositing)
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

# Encoded colour factors for D = B * M and T = 1 - (1 - D) * (1 - S).
# Bright diffuse channels join the screen layer with the specular term.
texture_light_factors_3d <- function(base, diffuse, target) {
  b <- texture_channels_3d(base)[1:3, , drop = FALSE] / 255
  d <- texture_channels_3d(diffuse)[1:3, , drop = FALSE] / 255
  t <- texture_channels_3d(target)[1:3, , drop = FALSE] / 255
  multiply <- pmin(d / pmax(b, 1 / 255), 1)
  intermediate <- b * multiply
  screen <- pmin(pmax((t - intermediate) / pmax(1 - intermediate, 1 / 255), 0), 1)
  image <- function(rgb) texture_pack_3d(rbind(round(rgb * 255), 255), nrow(base), ncol(base))
  list(multiply = image(multiply), screen = image(screen))
}

# Pixel-aligned bounds of a face run, including one pixel around its outline.
texture_crop_result_3d <- function(result, bounds) {
  extent <- result$extent
  w <- ncol(result$image); h <- nrow(result$image)
  span <- extent[3:4] - extent[1:2]
  cols <- pmax(1L, pmin(w, c(floor((bounds[1L] - extent[1L]) / span[1L] * w),
    ceiling((bounds[3L] - extent[1L]) / span[1L] * w) + 1L)))
  rows <- pmax(1L, pmin(h, c(floor((extent[4L] - bounds[4L]) / span[2L] * h),
    ceiling((extent[4L] - bounds[2L]) / span[2L] * h) + 1L)))
  pixels <- matrix(as.integer(result$image), h, w, byrow = TRUE)[seq.int(rows[1L], rows[2L]), seq.int(cols[1L], cols[2L]), drop = FALSE]
  result$image <- structure(as.integer(t(pixels)), dim = dim(pixels), class = "nativeRaster", channels = 4L)
  result$width <- ncol(pixels); result$height <- nrow(pixels)
  result$extent <- c(extent[1L] + (cols[1L] - 1L) / w * span[1L],
    extent[4L] - rows[2L] / h * span[2L], extent[1L] + cols[2L] / w * span[1L],
    extent[4L] - (rows[1L] - 1L) / h * span[2L])
  result
}

# A single clipped group per run keeps every lighting layer's interior
# continuous across fan triangles and across fragments of the same sheet.
texture_lighting_grob_3d <- function(scene, rows, viewport, light, name) {
  packet <- texture_run_packet_3d(scene, rows)
  camera <- scene$camera_input
  f <- scene$fragments
  counts <- f$count[rows]
  index <- rep(f$offset[rows], counts) + sequence(counts) - 1L
  x <- scene$vertices$x_npc[index]; y <- scene$vertices$y_npc[index]
  bounds <- c(min(x), min(y), max(x), max(y))
  render <- function(packet, light) texture_crop_result_3d(
    raster_scene_3d(packet, camera, viewport, 1L, light, blend = "srgb"), bounds)
  target <- render(packet, light)
  if (!texture_lighting_device_ok_3d()) {
    return(grid::gTree(children = grid::gList(raster_image_grob_3d(target)),
      name = name, texture_lighting = "hybrid"))
  }
  base <- render(packet, NULL)
  diffuse_packet <- packet
  diffuse_packet$materials$specular <- rep(0, nrow(diffuse_packet$materials))
  diffuse <- if (any(packet$materials$specular > 0)) render(diffuse_packet, light) else target
  factors <- texture_light_factors_3d(base$image, diffuse$image, target$image)
  opaque <- texture_channels_3d(base$image)
  opaque[4L, ] <- 255
  image <- function(native) { out <- base; out$image <- native; raster_image_grob_3d(out) }
  painted <- image(texture_pack_3d(opaque, nrow(base$image), ncol(base$image)))
  multiply <- getExportedValue("grid", "groupGrob")(image(factors$multiply), op = "multiply", dst = painted)
  screen <- getExportedValue("grid", "groupGrob")(image(factors$screen), op = "screen", dst = multiply)
  mask <- grid::as.mask(raster_image_grob_3d(base), type = "alpha")
  grid::gTree(children = grid::gList(screen), vp = grid::viewport(mask = mask),
    name = name, texture_lighting = "multiply-screen")
}

# Textured packets whose device cannot paint their affine or lighting groups
# join the panel's hybrid image, with vector text in the front pass.
texture_hybrid_packets_3d <- function(packets, light) {
  vapply(packets, function(packet) {
    textured <- !is.na(packet$styles$texture_id)
    any(textured) && (!texture_device_ok_3d() ||
      (!is.null(light) && any(textured & packet$styles$shade %in% TRUE) && !texture_lighting_device_ok_3d()))
  }, logical(1))
}
