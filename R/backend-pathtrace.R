utils::globalVariables(c("raymarch_path_prepare_cpp", "raymarch_path_cpp", "raymarch_path_finish_cpp"))
utils::globalVariables(c("raymarch_sampler_cpp", "raymarch_bsdf_cpp", "raymarch_medium_cpp"))

pathtrace_settings_3d <- function(samples, bounces, seed, aperture, focus, light_radius, progressive, batch_size,
                                  noise_threshold, min_samples, denoise, exposure, tone) {
  for (name in c("samples", "bounces", "seed", "batch_size", "min_samples")) {
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
  noise_threshold <- check_light_number_3d(noise_threshold, "noise_threshold")
  if (!is.logical(denoise) || length(denoise) != 1L || is.na(denoise)) stop("denoise must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(exposure) || length(exposure) != 1L || !is.finite(exposure) || abs(exposure) > 1023)
    stop("exposure must be a finite number of stops in [-1023, 1023].", call. = FALSE)
  list(samples = as.integer(samples), bounces = as.integer(bounces), seed = as.integer(seed),
    aperture = aperture, focus = focus, light_radius = light_radius, progressive = progressive,
    batch_size = as.integer(batch_size), noise_threshold = noise_threshold, min_samples = as.integer(min_samples),
    denoise = denoise, exposure = exposure, tone = match.arg(tone, c("clip", "aces", "agx")), state = new.env(parent = emptyenv()))
}

pathtrace_scene_3d <- function(packet, specs, camera, viewport, backend, light) {
  resolved <- scene_compile(new_scene_packet(), camera, viewport)$camera
  scale <- resolved$model_scale
  mm <- export_camera_3d(resolved, camera, viewport, 1)$mm_per_unit
  packet <- light_material_packet_3d(packet, light)
  nodes <- export_nodes_3d(scene_finite_packet_3d(packet, camera$pre), scale, NULL, mm,
    function(tab) paste(attr(tab, "kind"), tab$style_id, sep = ":"))
  mesh <- pathtrace_mesh_3d(nodes, packet, resolved, light)
  mesh$colour[, 1:3] <- srgb_decode_3d(mesh$colour[, 1:3, drop = FALSE])
  mesh$splats <- lapply(attr(packet, "splats"), splat_resolve_cpp, camera, viewport)
  fields <- lapply(specs, function(spec) {
    if (identical(spec$kind, "volume") && spec$mode == 1L) stop("MIP volumes are not physical; use absorption or iso for path tracing.", call. = FALSE)
    if (!spec$kind %in% c("sdf", "volume")) stop("Path tracing accepts mesh, SDF and scalar-volume layers.", call. = FALSE)
    if (identical(spec$kind, "sdf")) spec$rgb <- as.vector(srgb_decode_3d(colour_channels_3d(spec$fill) / 255))
    spec$material <- kernel_materials_3d(material_table_3d(spec$material %or3d% light$material)); spec
  })
  text_packet <- packet
  attr(text_packet, "splats") <- NULL
  empty <- new_scene_packet()
  for (kind in c("faces", "polylines", "markers", "quadrics")) text_packet[[kind]] <- empty[[kind]]
  result <- raster_scene_3d(text_packet, camera, viewport, 1L, NULL)
  settings <- backend[intersect(c("bounces", "seed", "aperture", "light_radius", "noise_threshold", "min_samples", "estimator"), names(backend))]
  settings$focus <- backend$focus %or3d% resolved$distance
  settings$epsilon <- 1e-6 * max(scale)
  sdf <- Filter(function(s) identical(s$kind, "sdf"), fields)
  if (length(sdf)) settings$epsilon <- max(settings$epsilon, vapply(sdf, function(s)
    4 * s$tolerance * max(scale / vapply(s$ranges, diff, numeric(1))), numeric(1)))
  light <- light %or3d% light_3d(light_directional(intensity = 0), ambient = 1)
  illumination <- resolved_light_3d(light, resolved, pathtrace = TRUE)
  key <- serialize(list(mesh, fields, camera, viewport, settings, illumination, result$extent), NULL, version = 2L)
  panel <- as.character(packet$panel_id)
  if (is.null(backend$state[[panel]])) backend$state[[panel]] <- new.env(parent = emptyenv())
  state <- backend$state[[panel]]
  if (!backend$progressive || !identical(state$key, key)) {
    state$key <- key; state$samples <- 0L
    state$pointer <- raymarch_path_prepare_cpp(mesh, fields, camera, viewport)
  }
  remaining <- backend$samples
  while (remaining > 0L) {
    count <- min(remaining, backend$batch_size)
    raymarch_path_cpp(state$pointer, as.integer(result$width), as.integer(result$height), result$extent,
      settings, illumination, state$samples, count, raster_threads_3d())
    state$samples <- state$samples + count
    remaining <- remaining - count
  }
  film <- raymarch_path_finish_cpp(state$pointer, backend$denoise)
  rgba <- film$rgba
  colour <- pathtrace_tone_3d(film$colour / pmax(rgba[, 4L], .Machine$double.eps), backend$exposure, backend$tone) * rgba[, 4L]
  result <- raymarch_paint_3d(result, colour, rgba[, 4L], seq_len(nrow(rgba)))
  result$samples <- state$samples
  result$sum <- film$sum
  result$counts <- film$counts; result$variance <- film$variance; result$features <- film$features
  result$linear <- film$colour
  result
}

# Illustrative surfaces terminate transport with the shared kernel's colour.
pathtrace_mesh_3d <- function(nodes, packet, camera, light) {
  options <- export_options_3d(FALSE, "tube", "mesh", 1)
  meshes <- lapply(nodes, function(node) export_merged_3d(list(nodes = list(node), up = 3L), options))
  mesh <- export_merged_3d(list(nodes = nodes, up = 3L), options)
  mesh$material <- unlist(Map(function(node, part) {
    key <- strsplit(node$name, ":", fixed = TRUE)[[1L]]
    id <- 0L
    if (key[1L] %in% c("faces", "quadrics")) {
      s <- packet$styles[match(as.integer(key[2L]), packet$styles$style_id), , drop = FALSE]
      m <- material_index_3d(s)
      if (m > 0L) id <- m
    }
    rep(id, ncol(part$triangles))
  }, nodes, meshes), use.names = FALSE)
  mesh$illustrative <- integer(length(mesh$material))
  if (!is.null(light) && nrow(packet$materials)) {
    id <- mesh$material; take <- id > 0L
    mesh$illustrative[take] <- ifelse(packet$materials$style[id[take]] != "standard", id[take], 0L)
  }
  mesh$materials <- kernel_materials_3d(packet$materials)
  mesh$lights <- if (is.null(light)) resolved_light_3d(light_3d(light_directional(intensity = 0), ambient = 1), camera) else resolved_light_3d(light, camera)
  mesh
}

render_pathtrace_panel_3d <- function(x, furniture, backend) {
  specs <- Filter(Negate(is.null), lapply(x$packets, attr, "raymarch"))
  packet <- merge_scene_packets(x$packets, x$panel_params$panel_id)
  result <- pathtrace_scene_3d(packet, specs, furniture$camera, furniture$viewport, backend,
    x$panel_params$coord_params$light)
  raster_panel_grob_3d(result, packet, furniture, "scene-pathtrace-3d")
}
