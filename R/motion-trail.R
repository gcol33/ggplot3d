motion_trail_spec_3d <- function(trail, time) {
  if (is.null(trail)) return(NULL)
  if (is.numeric(trail)) trail <- list(length = trail)
  allowed <- c("length", "radius", "width", "sides", "taper", "normal", "layers")
  scene_assert(is.list(trail) && !is.null(names(trail)) && !anyDuplicated(names(trail)) &&
    all(names(trail) %in% allowed), paste("trail must be a frame window or a list of", paste(allowed, collapse = ", ")))
  count <- trail$length
  scene_assert(is.numeric(count) && length(count) == 1L && is.finite(count) && count >= 1 &&
    count < .Machine$integer.max && count == trunc(count), "trail length must be a positive representable integer")
  scene_assert(!is.null(time), "trail requires time")
  for (name in c("radius", "width")) if (!is.null(trail[[name]])) {
    size <- trail[[name]]
    scene_assert(is.numeric(size) && length(size) == 1L && is.finite(size) && size > 0,
      paste("trail", name, "must be a positive finite number"))
  }
  scene_assert(is.null(trail$radius) || is.null(trail$width), "give either trail radius or width")
  trail$sides <- mesh_count_3d(trail$sides %or3d% 8L, "trail sides", 3L)
  trail$taper <- check_flag_3d(trail$taper %or3d% FALSE, "trail taper")
  if (!is.null(trail$normal)) scene_assert(is.numeric(trail$normal) && length(trail$normal) == 3L &&
    all(is.finite(trail$normal)) && any(trail$normal != 0), "trail normal must be a finite nonzero direction")
  trail
}

motion_trail_layers_3d <- function(plot, time, trail) {
  eligible <- unname(which(vapply(plot@layers, function(layer) {
    data <- if (is.data.frame(layer$data)) layer$data else plot@data
    entry <- find_emitter_3d(layer$geom)
    is.data.frame(data) && time %in% names(data) && identical(entry$tween, "row")
  }, logical(1))))
  if (is.null(trail$layers)) return(eligible)
  layers <- trail$layers
  scene_assert(is.numeric(layers) && length(layers) > 0L && all(is.finite(layers)) &&
    all(layers == trunc(layers)) && all(layers %in% eligible),
    "trail layers must index time-varying row-based layers")
  unique(as.integer(layers))
}

# Keep only anchor data for the current window; workers can start at any frame.
motion_trail_history_3d <- function(job, i) {
  indices <- seq.int(max(1, i - job$trail$length), i)
  cache <- job$trail_history
  stale <- setdiff(ls(cache), as.character(indices))
  if (length(stale)) rm(list = stale, envir = cache)
  spec <- job$tween %or3d% list(match = "group")
  for (index in indices) {
    key <- as.character(index)
    if (exists(key, cache, inherits = FALSE)) next
    built <- if (is.null(job$tween)) job$keyframes[[index]] else
      motion_tween_data_3d(job$keyframes, job$schedule, index, job$tween)
    cache[[key]] <- lapply(job$trail_layers, function(layer) {
      data <- built$data[[layer]]
      if (!nrow(data) || !all(c("x", "y", "z") %in% names(data))) return(NULL)
      data$.trail_key <- tween_row_keys_3d(data, spec)
      data$.trail_frame <- rep.int(index, nrow(data))
      data$.layer <- rep.int(layer, nrow(data))
      data <- data[intersect(names(data), c("x", "y", "z", "PANEL", ".source", ".layer",
        ".trail_key", ".trail_frame", "fill", "colour", "alpha", "radius", "width", "twist"))]
      data
    })
  }
  lapply(seq_along(job$trail_layers), function(layer) {
    parts <- lapply(indices, function(index) cache[[as.character(index)]][[layer]])
    data <- do.call(rbind, parts)
    if (is.null(data)) return(NULL)
    data <- tween_fade_3d(data, 1 - (i - data$.trail_frame) / job$trail$length)
    data$.trail_fraction <- 1 - (i - data$.trail_frame) / job$trail$length
    data
  })
}

motion_trail_curves_3d <- function(data, panel) {
  if (is.null(data) || !nrow(data)) return(list())
  data <- data[as.character(data$PANEL) == as.character(panel), , drop = FALSE]
  curves <- lapply(split(data, data$.trail_key), function(track) {
    valid <- is.finite(track$x) & is.finite(track$y) & is.finite(track$z)
    runs <- cumsum(c(TRUE, diff(track$.trail_frame) != 1L) | !valid | c(FALSE, head(!valid, -1L)))
    lapply(split(track[valid, , drop = FALSE], runs[valid]), function(curve) {
      xyz <- as.matrix(curve[c("x", "y", "z")])
      if (nrow(xyz) < 2L) return(NULL)
      keep <- c(rowSums(abs(diff(xyz))) != 0, TRUE)
      curve <- curve[keep, , drop = FALSE]
      if (nrow(curve) >= 2L) curve else NULL
    })
  })
  Filter(Negate(is.null), unlist(curves, recursive = FALSE))
}

motion_trail_packets_3d <- function(history, params, trail) {
  diagonal <- sqrt(sum(vapply(params$ranges[c("x", "y", "z")], function(range) diff(range)^2, numeric(1))))
  ribbon <- !is.null(trail$width)
  profile <- sweep_profile_3d(if (ribbon) rbind(c(0, -.5), c(0, .5)) else NULL, trail$sides, NULL)
  packets <- list()
  for (data in history) for (curve in motion_trail_curves_3d(data, params$panel_id)) {
    size <- if (ribbon) curve$width %or3d% rep(trail$width, nrow(curve)) else
      curve$radius %or3d% rep(trail$radius %or3d% (0.005 * diagonal), nrow(curve))
    if (trail$taper) size <- size * curve$.trail_fraction
    scene_assert(all(is.finite(size) & size >= 0), "trail cross-section sizes must be finite and nonnegative")
    fill <- curve$fill %or3d% rep(NA_character_, nrow(curve))
    colour <- curve$colour %or3d% rep("grey70", nrow(curve))
    curve$fill <- ifelse(is.na(fill), colour, fill)
    curve$colour <- NA_character_
    curve$group <- 1L
    surface <- sweep_surface_3d(curve, as.double(size),
      (curve$twist %or3d% rep(0, nrow(curve))) * pi / 180, profile, FALSE,
      if (ribbon) "none" else "flat", trail$normal)
    if (!nrow(surface)) next
    packets[[length(packets) + 1L]] <- emit_scene_3d(surface,
      list(.emitter = "GeomStatsSurface3D", shade = TRUE), params, curve$.layer[1L])
  }
  packets
}
