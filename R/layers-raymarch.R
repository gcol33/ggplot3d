GeomRaymarch3D <- ggplot2::ggproto("GeomRaymarch3D", ggplot2::Geom,
  required_aes = character(), default_aes = solid_defaults_3d(),
  draw_key = draw_key_glyph_3d
)

raymarch_payload_3d <- function(spec, params, panel_params) {
  if (!identical(frame_of_3d(panel_params)$identity, TRUE))
    stop("Ray-marched Euclidean layers require an identity coordinate frame.", call. = FALSE)
  if (!(panel_params$coord_params$backend$type %in% c("raster", "pathtrace")))
    stop("Ray-marched layers require a raster or pathtrace backend.", call. = FALSE)
  if (params$occlusion != "depth" || length(params$clip_planes))
    stop("Ray-marched layers require depth occlusion and the panel clipping box.", call. = FALSE)
  spec$ranges <- panel_params$ranges[c("x", "y", "z")]
  packet <- new_scene_packet(panel_id = panel_params$panel_id)
  attr(packet, "raymarch") <- spec
  attr(packet, "model_space") <- TRUE
  grid::gTree(packet = packet, cl = "scene_layer_grob")
}

emit_raymarch_3d <- function(data, params, panel_params) {
  if (nrow(data) != 1L) stop("A ray-marched layer requires one material row per panel.", call. = FALSE)
  spec <- params$ray_spec
  spec$fill <- data$fill[1L]; spec$alpha <- data$alpha[1L]
  if (is.na(spec$fill) || is.na(spec$alpha) || spec$alpha != 1 || colour_channels_3d(spec$fill, TRUE)[4L, 1L] != 255)
    stop("SDF trace surfaces require an opaque fill.", call. = FALSE)
  spec$layer_id <- data$.layer[1L]
  raymarch_payload_3d(spec, params, panel_params)
}

register_geom_3d(GeomRaymarch3D, emit_raymarch_3d, parameters = list(ray_spec = NULL))

raymarch_sdf_layer_3d <- function(mapping, data, fun, tolerance, max_steps, extra, show.legend, inherit.aes) {
  if (!inherits(fun, "ggplot3d_sdf")) stop("render = 'trace' requires a ggplot3d_sdf.", call. = FALSE)
  tolerance <- check_positive_3d(tolerance, "trace_tolerance")
  if (!is.numeric(max_steps) || length(max_steps) != 1L || !is.finite(max_steps) ||
      max_steps < 1 || max_steps != trunc(max_steps) || max_steps > .Machine$integer.max)
    stop("trace_steps must be a positive integer.", call. = FALSE)
  spec <- list(kind = "sdf", tape = sdf_compile_3d(fun), tolerance = tolerance, max_steps = as.integer(max_steps))
  make_layer_3d(GeomRaymarch3D, mapping, stats_seed_data_3d(data), "identity", "identity",
    stats_params_3d(list(ray_spec = spec), extra), show.legend, inherit.aes)
}
