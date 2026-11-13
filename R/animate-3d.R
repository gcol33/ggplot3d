#' Render a camera path
#'
#' Render a fixed scene from successive camera positions. Geometry and scales
#' are built once; projection, visibility and lighting are evaluated per frame.
#' @param plot A ggplot with [coord_3d()].
#' @param azimuth,elevation,zoom,roll A scalar, a numeric vector of length
#'   `frames`, or a function receiving times in seconds and returning either.
#'   NULL retains the plot setting, except azimuth, which makes one full turn.
#' @param frames Number of frames, default 60. The closing view is excluded
#'   from the default turn so that looping repeats each view once.
#' @param fps Frames per second, default 20.
#' @param file Output path ending in .gif or .mp4. NULL returns PNG frame paths.
#' @param renderer Scene backend, "raster" (default) or "vector".
#'   Both produce PNG frames for the encoder.
#' @param width,height Frame dimensions in pixels, default 640 each.
#' @param dpi Device resolution, default 96 dots per inch.
#' @param antialias Raster supersampling factor, default 1.
#' @param progress Report rendering time for each frame, default interactive().
#' @return A ggplot3d_animation list with `file`, `frame_count`, `fps`, `camera`
#'   (one row per frame), `timings` in seconds, `build_seconds`, `frame`,
#'   `panel_extents` in millimetres, and `frames` (PNG paths when file is NULL).
#'   PNG frames returned with file = NULL belong to the caller.
#' @details Framing is "azimuth" for constant elevation and roll, and "sphere"
#'   otherwise. Axis reserves and physical panel extents are fixed. Zoom remains
#'   an explicit magnification. GIF needs gifski; MP4 needs av. Optional packages
#'   are checked before rendering. Rendering streams frames to disk.
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
#'   geom_point_3d() + coord_3d()
#' if (requireNamespace("gifski", quietly = TRUE)) {
#'   movie <- animate_3d(p, frames = 3, width = 320, height = 320,
#'                       file = tempfile(fileext = ".gif"))
#'   movie$timings
#' }
#' @export
animate_3d <- function(plot, azimuth = NULL, elevation = NULL, zoom = NULL,
                       frames = 60L, fps = 20, file = NULL,
                       renderer = c("raster", "vector"), roll = NULL,
                       width = 640L, height = 640L, dpi = 96,
                       antialias = 1L, progress = interactive()) {
  renderer <- match.arg(renderer)
  motion_dimensions_3d(width, height, dpi)
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
  coord <- motion_plot_coord_3d(plot)
  times <- (seq_len(frames) - 1) / fps
  path <- list(azimuth = if (is.null(azimuth)) coord$camera$azimuth +
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
  built <- ggplot2::ggplotGrob(plot)
  panels <- motion_panels_3d(built)
  scene_assert(length(panels) > 0L, "plot has no 3D scene panels")
  build_seconds <- proc.time()[["elapsed"]] - start
  timings <- numeric(frames)
  extents <- vector("list", frames)
  for (i in seq_len(frames)) {
    start <- proc.time()[["elapsed"]]
    if (i > 1L) {
      device <- motion_png_3d(paths[i], width, height, dpi)
    }
    registry <- new.env(parent = emptyenv())
    current <- motion_map_panels_3d(built, function(panel) {
      params <- panel$panel_params
      camera <- cameras[[i]]
      camera$aspect <- params$camera$aspect
      params$camera <- camera
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
    camera = data.frame(time = times, path), timings = timings,
    build_seconds = build_seconds, frame = frame, panel_extents = extents,
    frames = if (is.null(file)) paths else character()), class = "ggplot3d_animation")
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
  camera$clip <- if (camera$clip) "on" else "off"
  camera$aspect <- coord$model_aspect
  do.call(coord_3d, c(camera, list(backend = backend, frame = frame)))
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
