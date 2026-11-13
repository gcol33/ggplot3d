#' Render a camera path or data that change over time
#'
#' Render a scene from successive camera positions, or draw one subset of the
#' data per frame. Projection, visibility and lighting are evaluated per frame.
#' @param plot A ggplot with [coord_3d()].
#' @param azimuth,elevation,zoom,roll A scalar, a numeric vector of length
#'   `frames`, or a function receiving times in seconds and returning either.
#'   NULL retains the plot setting, except azimuth without `time`, which makes
#'   one full turn.
#' @param frames Number of frames, default 60. The closing view is excluded
#'   from the default turn so that looping repeats each view once. With `time`,
#'   one frame per distinct value of the time column.
#' @param time Name of a data column holding the frame of each row, or NULL
#'   (default) for a fixed scene. Frame `i` draws the rows whose value equals
#'   the `i`-th sorted distinct value; layers without the column appear in
#'   every frame. Scales are trained on all frames, so limits, breaks and colour
#'   mappings are the same in each frame.
#' @param fps Frames per second, default 20.
#' @param file Output path ending in .gif or .mp4. NULL returns PNG frame paths.
#' @param renderer Scene backend, "raster" (default) or "vector".
#'   Both produce PNG frames for the encoder.
#' @param width,height Frame dimensions in pixels, default 640 each.
#' @param dpi Device resolution, default 96 dots per inch.
#' @param antialias Raster supersampling factor, default 1.
#' @param progress Report rendering time for each frame, default interactive().
#' @return A ggplot3d_animation list with `file`, `frame_count`, `fps`, `camera`
#'   (one row per frame, with the frame's `value` of the time column when
#'   `time` is set), `timings` in seconds, `build_seconds`, `frame`,
#'   `panel_extents` in millimetres, and `frames` (PNG paths when file is NULL).
#'   PNG frames returned with file = NULL belong to the caller.
#' @details Framing is "azimuth" for constant elevation and roll, and "sphere"
#'   otherwise. Axis reserves and physical panel extents are fixed. Zoom remains
#'   an explicit magnification. GIF needs gifski; MP4 needs av. Optional packages
#'   are checked before rendering. Rendering streams frames to disk. A fixed
#'   scene is built once; with `time`, each frame is built from its subset with
#'   the scale limits of the full data, as in gganimate's `transition_manual()`
#'   and plotly's `frame` aesthetic.
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
#'   geom_point_3d() + coord_3d()
#' if (requireNamespace("gifski", quietly = TRUE)) {
#'   movie <- animate_3d(p, frames = 3, width = 320, height = 320,
#'                       file = tempfile(fileext = ".gif"))
#'   movie$timings
#' }
#' waves <- expand.grid(x = seq(0, 1, length.out = 20), y = seq(0, 1, length.out = 20),
#'                      t = seq(0, 0.75, by = 0.25))
#' waves$z <- sin(pi * waves$x) * sin(pi * waves$y) * cos(2 * pi * waves$t)
#' wave <- ggplot2::ggplot(waves, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d() + coord_3d()
#' if (requireNamespace("gifski", quietly = TRUE)) {
#'   movie <- animate_3d(wave, time = "t", width = 320, height = 320,
#'                       file = tempfile(fileext = ".gif"))
#'   movie$camera$value
#' }
#' @export
animate_3d <- function(plot, azimuth = NULL, elevation = NULL, zoom = NULL,
                       frames = 60L, fps = 20, file = NULL,
                       renderer = c("raster", "vector"), roll = NULL,
                       width = 640L, height = 640L, dpi = 96,
                       antialias = 1L, progress = interactive(), time = NULL) {
  renderer <- match.arg(renderer)
  motion_dimensions_3d(width, height, dpi)
  coord <- motion_plot_coord_3d(plot)
  values <- NULL
  if (!is.null(time)) {
    values <- motion_time_values_3d(plot, time)
    if (missing(frames)) frames <- length(values)
    scene_assert(identical(as.numeric(frames), as.numeric(length(values))),
      sprintf("frames must equal the %d distinct values of '%s'", length(values), time))
  }
  scene_assert(is.numeric(frames) && length(frames) == 1L && is.finite(frames) &&
    frames >= 1 && frames == trunc(frames), "frames must be a positive integer")
  scene_assert(is.numeric(fps) && length(fps) == 1L && is.finite(fps) && fps > 0,
    "fps must be a positive finite number")
  scene_assert(is.logical(progress) && length(progress) == 1L && !is.na(progress),
    "progress must be TRUE or FALSE")
  backend <- backend_3d(renderer, dpi, antialias)
  extension <- NULL
  if (!is.null(file)) {
    scene_assert(is.character(file) && length(file) == 1L && !is.na(file) && nzchar(file),
      "file must be a single output path")
    extension <- tolower(tools::file_ext(file))
    scene_assert(extension %in% c("gif", "mp4"), "file must end in .gif or .mp4")
    motion_require_3d(if (extension == "gif") "gifski" else "av")
    scene_assert(dir.exists(dirname(file)), "The output directory does not exist")
  }
  times <- (seq_len(frames) - 1) / fps
  path <- list(azimuth = if (is.null(azimuth) && is.null(time)) coord$camera$azimuth +
    360 * (seq_len(frames) - 1) / frames else azimuth,
    elevation = elevation, zoom = zoom, roll = roll)
  path <- lapply(names(path), function(key) {
    value <- path[[key]]
    if (is.null(value)) value <- coord$camera[[key]]
    if (is.function(value)) value <- value(times)
    scene_assert(is.numeric(value) && length(value) %in% c(1L, frames) &&
      all(is.finite(value)), paste(key, "must return one or frames finite values"))
    rep_len(as.double(value), frames)
  }) |> stats::setNames(names(path))
  frame <- if (length(unique(path$elevation)) == 1L &&
    length(unique(path$roll)) == 1L) "azimuth" else "sphere"
  cameras <- lapply(seq_len(frames), function(i) {
    motion_coord_3d(coord, lapply(path, `[`, i), backend, frame)$camera
  })
  start <- proc.time()[["elapsed"]]
  directory <- tempfile("ggplot3d-frames-")
  dir.create(directory)
  complete <- FALSE
  on.exit(if (!complete || !is.null(file)) unlink(directory, recursive = TRUE), add = TRUE)
  paths <- file.path(directory, sprintf("frame-%06d.png", seq_len(frames)))
  # The device fixes text measurements before the grammar is built.
  motion_require_3d("ragg")
  device <- motion_png_3d(paths[1L], width, height, dpi)
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE, after = FALSE)
  limits <- if (is.null(time)) NULL else motion_frame_limits_3d(plot, time, values)
  build <- function(i) {
    if (is.null(time)) return(ggplot2::ggplotGrob(plot))
    ggplot2::ggplotGrob(motion_frame_plot_3d(plot, time, values[[i]], limits))
  }
  built <- build(1L)
  panels <- motion_panels_3d(built)
  scene_assert(length(panels) > 0L, "plot has no 3D scene panels")
  build_seconds <- proc.time()[["elapsed"]] - start
  timings <- numeric(frames)
  extents <- vector("list", frames)
  for (i in seq_len(frames)) {
    start <- proc.time()[["elapsed"]]
    if (i > 1L) {
      device <- motion_png_3d(paths[i], width, height, dpi)
      if (!is.null(time)) built <- build(i)
    }
    registry <- new.env(parent = emptyenv())
    current <- motion_map_panels_3d(built, function(panel) {
      params <- panel$panel_params
      params$camera <- motion_panel_camera_3d(cameras[[i]], params$camera, coord$camera)
      params$coord_params$backend <- backend
      params$coord_params$frame <- frame
      params$coord_params$frame_registry <- registry
      scene_panel_grob(panel$packets, params, panel$theme, panel$underlay, panel$overlay)
    })
    forced <- grid::grid.force(current)
    grid::grid.draw(forced)
    extents[[i]] <- lapply(motion_panels_3d(forced), function(panel) {
      c(width_mm = panel$inner_viewport$width_mm,
        height_mm = panel$inner_viewport$height_mm)
    })
    grDevices::dev.off(device)
    timings[i] <- proc.time()[["elapsed"]] - start
    if (progress) message(sprintf("Frame %d/%d: %.3f s", i, frames, timings[i]))
  }
  if (!is.null(file)) {
    if (extension == "gif") gifski::gifski(paths, gif_file = file,
      width = width, height = height, delay = 1 / fps, progress = progress)
    else av::av_encode_video(paths, output = file, framerate = fps, verbose = progress)
  }
  complete <- TRUE
  structure(list(file = file, frame_count = as.integer(frames), fps = fps,
    camera = if (is.null(time)) data.frame(time = times, path) else
      data.frame(time = times, value = values, path), timings = timings,
    build_seconds = build_seconds, frame = frame, panel_extents = extents,
    frames = if (is.null(file)) paths else character()), class = "ggplot3d_animation")
}

motion_layer_data_3d <- function(plot) {
  data <- lapply(plot$layers, function(layer) layer$data)
  c(list(plot$data), Filter(is.data.frame, data))
}

motion_time_values_3d <- function(plot, time) {
  scene_assert(is.character(time) && length(time) == 1L && !is.na(time) && nzchar(time),
    "time must be the name of a data column")
  data <- Filter(function(d) is.data.frame(d) && time %in% names(d), motion_layer_data_3d(plot))
  scene_assert(length(data) > 0L, sprintf("no layer data has a column '%s'", time))
  values <- sort(unique(do.call(c, lapply(data, function(d) d[[time]]))))
  scene_assert(length(values) > 0L && !anyNA(values), sprintf("'%s' must have non-missing values", time))
  values
}

# Statistics run on each frame's own rows, so every frame is built once and its
# trained limits are merged. Non-position scales are global: their limits become
# the union of ranges (continuous) or levels (discrete). Position scales are
# trained per panel, so free facets keep their own ranges: a blank layer carries
# each panel's position extremes over all frames, with the panel's facet values,
# and every frame trains on it.
motion_frame_limits_3d <- function(plot, time, values) {
  frames <- lapply(values, function(value) {
    ggplot2::ggplot_build(motion_frame_plot_3d(plot, time, value, NULL))
  })
  scales <- frames[[1L]]$plot$scales$clone()
  for (other in frames[-1L]) for (scale in other$plot$scales$scales) {
    if (!scales$has_scale(scale$aesthetics[1L])) scales$add(scale$clone())
  }
  positions <- c("x", "y", "z")
  for (scale in scales$scales) {
    if (scale$aesthetics[1L] %in% positions) next
    limits <- lapply(frames, function(frame) {
      match <- frame$plot$scales$get_scales(scale$aesthetics[1L])
      if (is.null(match)) NULL else match$get_limits()
    })
    limits <- unlist(Filter(Negate(is.null), limits))
    scale$limits <- if (scale$is_discrete()) unique(limits) else range(limits, na.rm = TRUE)
  }
  blank <- do.call(rbind, lapply(frames, motion_panel_extremes_3d))
  list(scales = scales, blank = blank)
}

motion_panel_extremes_3d <- function(built) {
  layout <- built$layout$layout
  facets <- setdiff(names(layout), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "SCALE_Z"))
  rows <- lapply(seq_len(nrow(layout)), function(i) {
    columns <- lapply(c(x = "x", y = "y", z = "z"), function(aes) {
      panel_scales <- built$layout[[paste0("panel_scales_", aes)]]
      index <- layout[[paste0("SCALE_", toupper(aes))]][i]
      if (is.null(panel_scales) || is.null(index)) return(NULL)
      scale <- panel_scales[[index]]
      limits <- scale$get_limits()
      if (scale$is_discrete()) as.character(limits) else scale$get_transformation()$inverse(limits)
    })
    columns <- Filter(Negate(is.null), columns)
    n <- max(lengths(columns))
    frame <- as.data.frame(lapply(columns, function(v) c(v, rep(v[length(v)], n - length(v)))),
      stringsAsFactors = FALSE)
    for (facet in facets) frame[[facet]] <- rep(layout[[facet]][i], n)
    frame
  })
  do.call(rbind, rows)
}

motion_frame_plot_3d <- function(plot, time, value, limits) {
  keep <- function(d) if (is.data.frame(d) && time %in% names(d)) d[d[[time]] == value, , drop = FALSE] else d
  plot$data <- keep(plot$data)
  plot$layers <- lapply(plot$layers, function(layer) {
    if (!is.data.frame(layer$data)) return(layer)
    layer <- ggplot2::ggproto(NULL, layer)
    layer$data <- keep(layer$data)
    layer
  })
  if (is.null(limits)) return(plot)
  plot$scales <- limits$scales$clone()
  mapping <- lapply(intersect(c("x", "y", "z"), names(limits$blank)), as.name)
  names(mapping) <- intersect(c("x", "y", "z"), names(limits$blank))
  plot + ggplot2::geom_blank(data = limits$blank, mapping = do.call(ggplot2::aes, mapping), inherit.aes = FALSE)
}

motion_require_3d <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("Install the '%s' package to use this output.", package), call. = FALSE)
  }
}

motion_dimensions_3d <- function(width, height, dpi) {
  scene_assert(all(vapply(list(width, height), function(x) is.numeric(x) &&
    length(x) == 1L && is.finite(x) && x >= 1 && x == trunc(x), logical(1))),
    "width and height must be positive integer pixels")
  scene_assert(is.numeric(dpi) && length(dpi) == 1L && is.finite(dpi) && dpi > 0,
    "dpi must be a positive finite number")
}

motion_png_3d <- function(file, width, height, dpi) {
  ragg::agg_png(file, width = width, height = height, res = dpi, units = "px")
  device <- grDevices::dev.cur()
  scene_assert(device > 1L, "could not open a PNG device for animation frames")
  device
}

motion_plot_coord_3d <- function(plot) {
  scene_assert(inherits(plot, "ggplot") && inherits(plot$coordinates, "Coord3D"),
    "plot must be a ggplot with coord_3d()")
  plot$coordinates
}

motion_coord_3d <- function(coord, values, backend, frame) {
  camera <- unclass(coord$camera)
  for (key in names(values)) camera[[key]] <- values[[key]]
  for (key in c("distance", "near", "far")) {
    if (is.na(camera[[key]])) camera[key] <- list(NULL)
  }
  # A copy of the coord keeps its frame, limits and theme settings.
  config <- coord$config
  config$backend <- backend
  config$frame <- match.arg(frame, c("view", "azimuth", "sphere"))
  config$frame_registry <- new.env(parent = emptyenv())
  ggplot2::ggproto(NULL, coord, camera = do.call(new_camera_3d, camera), config = config)
}

# A panel with its own view moves with the animated camera and keeps its
# offset from the coord's first view: azimuth, elevation and roll differences
# add, the zoom ratio multiplies.
motion_panel_camera_3d <- function(camera, panel, base) {
  camera$aspect <- panel$aspect
  camera$azimuth <- camera$azimuth + panel$azimuth - base$azimuth
  camera$elevation <- max(-90, min(90, camera$elevation + panel$elevation - base$elevation))
  camera$roll <- camera$roll + panel$roll - base$roll
  camera$zoom <- camera$zoom * panel$zoom / base$zoom
  camera
}

motion_panels_3d <- function(grob) {
  if (inherits(grob, "scene_panel_grob")) return(list(grob))
  children <- if (length(grob$children)) as.list(grob$children) else grob$grobs
  unlist(lapply(children, motion_panels_3d), recursive = FALSE)
}

motion_map_panels_3d <- function(grob, f) {
  if (inherits(grob, "scene_panel_grob")) return(f(grob))
  if (length(grob$children)) {
    grob$children <- do.call(grid::gList, lapply(as.list(grob$children), motion_map_panels_3d, f))
  } else if (length(grob$grobs)) grob$grobs <- lapply(grob$grobs, motion_map_panels_3d, f)
  grob
}
