utils::globalVariables(c("raymarch_path_prepare_cpp", "raymarch_path_cpp"))

pathtrace_settings_3d <- function(samples, bounces, seed, aperture, focus, light_radius, progressive, batch_size) {
  for (name in c("samples", "bounces", "seed", "batch_size")) {
    value <- get(name)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value != trunc(value) ||
        value < (if (name == "seed") 0 else 1) || value > .Machine$integer.max)
      stop(name, " must be a representable ", if (name == "seed") "nonnegative" else "positive", " integer.", call. = FALSE)
  }
  aperture <- check_positive_3d(aperture, "aperture", zero = TRUE)
  if (!is.null(focus)) focus <- check_positive_3d(focus, "focus")
  light_radius <- check_positive_3d(light_radius, "light_radius", zero = TRUE)
  if (light_radius >= 90) stop("light_radius must be below 90 degrees.", call. = FALSE)
  if (!is.logical(progressive) || length(progressive) != 1L || is.na(progressive)) stop("progressive must be TRUE or FALSE.", call. = FALSE)
  list(samples = as.integer(samples), bounces = as.integer(bounces), seed = as.integer(seed),
    aperture = aperture, focus = focus, light_radius = light_radius, progressive = progressive,
    batch_size = as.integer(batch_size), state = new.env(parent = emptyenv()))
}

pathtrace_scene_3d <- function(packet, specs, camera, viewport, backend, light) {
  resolved <- scene_compile(new_scene_packet(), camera, viewport)$camera
  scale <- resolved$model_scale
  mm <- export_camera_3d(resolved, camera, viewport, 1)$mm_per_unit
  nodes <- export_nodes_3d(scene_finite_packet_3d(packet, camera$pre), scale, NULL, mm,
    function(tab) rep("scene", length(tab$offset)))
  mesh <- export_merged_3d(list(nodes = nodes, up = 3L), export_options_3d(FALSE, "tube", "mesh", 1))
  mesh$colour[, 1:3] <- srgb_decode_3d(mesh$colour[, 1:3, drop = FALSE])
  fields <- lapply(specs, function(spec) {
    if (!identical(spec$kind, "sdf")) stop("Path tracing currently accepts mesh and SDF surfaces; use the raster backend for scalar volumes.", call. = FALSE)
    spec$rgb <- as.vector(srgb_decode_3d(colour_channels_3d(spec$fill) / 255)); spec
  })
  text_packet <- packet
  empty <- new_scene_packet()
  for (kind in c("faces", "polylines", "markers")) text_packet[[kind]] <- empty[[kind]]
  result <- raster_scene_3d(text_packet, camera, viewport, 1L, NULL)
  settings <- backend[c("bounces", "seed", "aperture", "light_radius")]
  settings$focus <- backend$focus %or3d% resolved$distance
  settings$epsilon <- 1e-6 * max(scale)
  if (length(fields)) settings$epsilon <- max(settings$epsilon, vapply(fields, function(s)
    4 * s$tolerance * max(scale / vapply(s$ranges, diff, numeric(1))), numeric(1)))
  light <- light %or3d% light_3d(ambient = 1, diffuse = 0)
  direction <- light$direction
  if (light$space == "camera") direction <- resolved$right * direction[1L] + resolved$up * direction[2L] - resolved$forward * direction[3L]
  else direction <- direction * scale
  illumination <- c(direction / sqrt(sum(direction^2)), light$ambient, light$diffuse)
  key <- serialize(list(mesh, fields, camera, viewport, settings, illumination, result$extent), NULL, version = 2L)
  panel <- as.character(packet$panel_id)
  if (is.null(backend$state[[panel]])) backend$state[[panel]] <- new.env(parent = emptyenv())
  state <- backend$state[[panel]]
  if (!backend$progressive || !identical(state$key, key)) {
    state$key <- key; state$samples <- 0L
    state$sum <- matrix(0, result$width * result$height, 4L)
    state$pointer <- raymarch_path_prepare_cpp(mesh, fields, camera, viewport)
  }
  remaining <- backend$samples
  while (remaining > 0L) {
    count <- min(remaining, backend$batch_size)
    state$sum <- raymarch_path_cpp(state$pointer, as.integer(result$width), as.integer(result$height), result$extent,
      settings, illumination, state$samples, count, state$sum, raster_threads_3d())
    state$samples <- state$samples + count
    remaining <- remaining - count
  }
  rgba <- state$sum / state$samples
  result <- raymarch_paint_3d(result, rgba[, 1:3, drop = FALSE], rgba[, 4L], seq_len(nrow(rgba)))
  result$samples <- state$samples
  result$sum <- state$sum
  result
}

render_pathtrace_panel_3d <- function(x, furniture, backend) {
  specs <- Filter(Negate(is.null), lapply(x$packets, attr, "raymarch"))
  packet <- merge_scene_packets(x$packets, x$panel_params$panel_id)
  result <- pathtrace_scene_3d(packet, specs, furniture$camera, furniture$viewport, backend,
    x$panel_params$coord_params$light)
  raster_panel_grob_3d(result, packet, furniture, "scene-pathtrace-3d")
}
