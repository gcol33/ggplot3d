#' Render a camera path or data that change over time
#'
#' Render a scene from successive camera positions, or draw one subset of the
#' data per frame. Projection, visibility and lighting are evaluated per frame.
#' @param plot A ggplot with [coord_3d()].
#' @param azimuth,elevation,zoom,roll A scalar, a numeric vector of length
#'   `frames`, or a function receiving times in seconds and returning either.
#'   NULL retains the plot setting, except azimuth without `time` or `path`,
#'   which makes one full turn.
#' @param frames Number of frames, default 60. The closing view is excluded
#'   from the default turn so that looping repeats each view once. With `time`,
#'   one frame per distinct value of the time column. With `path`, the default
#'   samples the path at `fps` frames per second, and a given number of frames
#'   is spread evenly over the path.
#' @param time Name of a data column holding the frame of each row, or NULL
#'   (default) for a fixed scene. Frame `i` draws the rows whose value equals
#'   the `i`-th sorted distinct value; layers without the column appear in
#'   every frame. Scales are trained on all frames, so limits, breaks and colour
#'   mappings are the same in each frame.
#' @param fps Frames per second, default 20.
#' @param file Output path, or NULL to return PNG frame paths. The extension
#'   selects the output: `.gif` (needs gifski), `.mp4` or `.webm` (need av),
#'   `.apng` (an animated PNG), `.png` or `.svg` (one file per frame; svg needs
#'   svglite) and `.pdf` (one page per frame). A file per frame takes its
#'   number from a [sprintf()] pattern in the name, such as
#'   `"frames/turn-%03d.png"`, or else gets it appended, as in
#'   `turn-0001.png`.
#'   `.glb` and `.gltf` hold the first panel as an animated 3D scene for
#'   Blender and other glTF software: camera translation, rotation and zoom
#'   tracks, with per-frame visibility tracks for changing geometry.
#'   Each glTF animation uses one projection type; export perspective and
#'   orthographic paths separately.
#' @param renderer Scene backend, "raster" (default) or "vector". Use "vector"
#'   for vector pages in SVG and PDF output.
#' @param width,height Frame dimensions in pixels, default 640 each.
#' @param dpi Device resolution, default 96 dots per inch. SVG and PDF pages
#'   measure `width / dpi` by `height / dpi` inches.
#' @param antialias Raster supersampling factor, default 1.
#' @param progress Report rendering time for each frame, default interactive().
#' @param path A camera path from [camera_path_3d()], [orbit_3d()],
#'   [fly_through_3d()] or [dolly_zoom_3d()], or NULL. A path replaces
#'   `azimuth`, `elevation`, `zoom` and `roll` and also moves the target,
#'   distance, field of view and projection.
#' @param workers Number of processes drawing frames, default 1, or a cluster
#'   from [parallel::makeCluster()] with ggplot3d loaded on its nodes. Each
#'   worker draws a contiguous run of frames on its own devices, so the frames
#'   are identical for every number of workers. Workers fork where the
#'   platform allows it and otherwise start socket processes, which load the
#'   installed ggplot3d. PDF pages are drawn in one process.
#' @param ... Arguments to [export_3d()] for glTF output: `furniture`, `lines`,
#'   `points` and `size`.
#' @param tween A [tween_3d()] specification for in-between data frames, or NULL
#'   for one frame per state. Requires `time`. The number of frames is then
#'   `(number of states - 1) * (tween$n + 1) + 1`.
#' @param trail NULL (default), a positive integer giving the number of preceding
#'   rendered frames in a motion trail, or a list with `length` and optional
#'   `radius`, `width`, `sides` (default 8), `taper` (default FALSE), `normal`
#'   and `layers` (layer indices). Trails follow the anchors of time-varying
#'   row-based marks, including points, text and solid glyphs. Map `group` to a
#'   persistent object identifier. Trails use [geom_tube_3d()]'s sweep; setting
#'   `width` selects a ribbon. Cross-section sizes use transformed position
#'   units; the default radius is 0.5% of the panel range diagonal. Mapped
#'   `radius`, `width` and `twist` are retained. Alpha fades linearly with frame
#'   age, multiplying the mark's alpha. `taper = TRUE` also scales the cross
#'   section by this fraction. Requires `time`; tweened frames count towards
#'   the window. Static layers stay fixed. Missing observations break a trail.
#' @param isometry A list of one canonical-space isometry matrix per frame,
#'   a matrix shared by all frames, or a function of frame times returning the
#'   list. Re-emits geometry through the space adapter; an isometry that acts
#'   linearly on the display model (Lorentz transformations in the Klein
#'   model) changes only the camera pre-transform. Limits remain fixed.
#' @param tour A [tour_grand()] specification or tourr history array. Applies
#'   Grassmann-interpolated bases to stat_project_3d() layers with orthographic
#'   or finite perspective projection. Uses fixed limits across frames. Requires static
#'   input data, linear position scales, and no tween or trail.
#' @return A ggplot3d_animation list with `file`, `frame_count`, `fps`, `camera`
#'   (one row per frame with the time, the camera angles, zoom, projection,
#'   field of view, distance (NA when automatic) and target, and the frame's
#'   `value` of the time column when `time` is set), `timings` in seconds,
#'   `transition` (source state, destination state and eased fraction when
#'   tweening),
#'   `build_seconds`, `frame`, `panel_extents` in millimetres, and `frames`
#'   (the PNG paths when file is NULL, the written files of a per-frame
#'   output). PNG frames returned with file = NULL belong to the caller.
#' @details Framing is "azimuth" when only the azimuth and zoom change, and
#'   "sphere" otherwise. Axis reserves and physical panel extents are fixed.
#'   Zoom remains an explicit magnification. Optional packages are checked
#'   before rendering. Rendering streams frames to disk. A fixed scene is built
#'   once; with `time`, each frame is built from its subset with the scale
#'   limits of the full data, as in gganimate's `transition_manual()` and
#'   plotly's `frame` aesthetic. The animated PNG follows the APNG 1.0
#'   specification: browsers play it and viewers without APNG support show its
#'   first frame.
#'
#'   gganimate's `transition_states()` and `transition_time()` use a separate
#'   build pipeline. These transitions produce guidance to use
#'   `animate_3d(time = , tween = tween_3d())` for a 3D plot.
#' @seealso [camera_path_3d()] for keyframed camera moves, [view_3d()] to play
#'   a path in the browser.
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
#'   geom_point_3d() + coord_3d()
#' if (requireNamespace("gifski", quietly = TRUE)) {
#'   movie <- animate_3d(p, frames = 3, width = 320, height = 320,
#'                       file = tempfile(fileext = ".gif"))
#'   movie$timings
#' }
#' if (requireNamespace("ragg", quietly = TRUE)) {
#'   turn <- animate_3d(p, path = orbit_3d(degrees = 90, duration = 1), fps = 3,
#'                      width = 240, height = 240, file = tempfile(fileext = ".apng"))
#'   turn$camera$azimuth
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
#' track <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0), z = c(0, 0.5, 1), t = 1:3)
#' moving <- ggplot2::ggplot(track, ggplot2::aes(x, y, z = z, group = 1)) +
#'   geom_point_3d(colour = "steelblue", size = 3) + coord_3d()
#' if (requireNamespace("ragg", quietly = TRUE)) {
#'   wake <- animate_3d(moving, time = "t", tween = tween_3d(n = 1),
#'     trail = list(length = 3, radius = 0.04, taper = TRUE),
#'     width = 240, height = 240, progress = FALSE)
#'   unlink(wake$frames)
#' }
#' @export
animate_3d <- function(plot, azimuth = NULL, elevation = NULL, zoom = NULL,
                       frames = 60L, fps = 20, file = NULL,
                       renderer = c("raster", "vector"), roll = NULL,
                       width = 640L, height = 640L, dpi = 96,
                       antialias = 1L, progress = interactive(), time = NULL,
                       path = NULL, workers = 1L, tween = NULL, trail = NULL, isometry = NULL, tour = NULL, ...) {
  renderer <- match.arg(renderer)
  motion_dimensions_3d(width, height, dpi)
  coord <- motion_plot_coord_3d(plot)
  trail <- motion_trail_spec_3d(trail, time)
  if (!is.null(tween)) {
    scene_assert(inherits(tween, "ggplot3d_tween") && !is.null(time), "tween must be a tween_3d() specification with time set")
  }
  if (!is.null(tween) || !is.null(trail)) {
    plot <- tween_plot_3d(plot)
    coord <- plot@coordinates
  }
  if (!is.null(path)) {
    scene_assert(inherits(path, "ggplot3d_camera_path"),
      "path must be a camera path such as camera_path_3d() or orbit_3d()")
    scene_assert(is.null(azimuth) && is.null(elevation) && is.null(zoom) && is.null(roll),
      "give either path or azimuth, elevation, zoom and roll")
  }
  frames_given <- !missing(frames)
  values <- NULL
  states <- NULL; schedule <- NULL
  if (!is.null(time)) {
    states <- motion_time_values_3d(plot, time)
    if (!is.null(tween)) {
      schedule <- tween_schedule_3d(states, tween)
      values <- if (is.numeric(states)) states[schedule$from] * (1 - schedule$fraction) + states[schedule$to] * schedule$fraction
        else states[ifelse(schedule$fraction < 0.5, schedule$from, schedule$to)]
    } else values <- states
    if (!frames_given) frames <- length(values)
    frames_given <- TRUE
    scene_assert(identical(as.numeric(frames), as.numeric(length(values))),
      if (is.null(tween)) sprintf("frames must equal the %d distinct values of '%s'", length(values), time)
        else sprintf("frames must equal the %d scheduled data frames", length(values)))
  }
  scene_assert(is.numeric(frames) && length(frames) == 1L && is.finite(frames) &&
    frames >= 1 && frames == trunc(frames), "frames must be a positive integer")
  scene_assert(is.numeric(fps) && length(fps) == 1L && is.finite(fps) && fps > 0,
    "fps must be a positive finite number")
  scene_assert(is.logical(progress) && length(progress) == 1L && !is.na(progress),
    "progress must be TRUE or FALSE")
  workers <- motion_workers_3d(workers)
  backend <- backend_3d(renderer, dpi, antialias)
  output <- motion_output_3d(file)
  scene_assert(!...length() || output$kind == "scene", "Further arguments apply to .glb and .gltf output only")
  options <- if (output$kind == "scene") motion_export_options_3d(...)
  if (is.null(path)) {
    times <- (seq_len(frames) - 1) / fps
    steps <- motion_track_steps_3d(coord, list(azimuth = if (is.null(azimuth) && is.null(time) && is.null(isometry) && is.null(tour))
      coord$camera$azimuth + 360 * (seq_len(frames) - 1) / frames else azimuth,
      elevation = elevation, zoom = zoom, roll = roll), frames, times)
    cameras <- lapply(steps, motion_camera_3d, base = coord$camera)
  }
  start <- proc.time()[["elapsed"]]
  directory <- tempfile("ggplot3d-frames-")
  dir.create(directory)
  complete <- FALSE
  on.exit(if (!complete || !is.null(file)) unlink(directory, recursive = TRUE), add = TRUE)
  # The device fixes text measurements before the grammar is built.
  device <- motion_open_device_3d(output$device, file.path(directory, paste0("measure.", output$device)),
    width, height, dpi)
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE, after = FALSE)
  limits <- if (is.null(time)) NULL else motion_frame_limits_3d(plot, time, states)
  keyframes <- if (is.null(tween) && is.null(trail)) NULL else lapply(states, function(value) {
    ggplot2::ggplot_build(motion_frame_plot_3d(plot, time, value, limits))
  })
  built <- if (!is.null(keyframes)) ggplot2::ggplot_gtable(keyframes[[1L]]) else
    ggplot2::ggplotGrob(if (is.null(time)) plot else motion_frame_plot_3d(plot, time, values[[1L]], limits))
  panels <- motion_panels_3d(built)
  scene_assert(length(panels) > 0L, "plot has no 3D scene panels")
  build_seconds <- proc.time()[["elapsed"]] - start
  if (!is.null(path)) {
    context <- motion_path_context_3d(coord$camera, panels[[1L]]$panel_params,
      motion_ratio_3d(built, coord$camera, backend))
    sampled <- path_sample_3d(path, if (frames_given) frames else NULL, fps, context)
    frames <- length(sampled$times)
    times <- sampled$times
    steps <- sampled$steps
    cameras <- lapply(steps, motion_camera_3d, base = coord$camera)
  }
  tour_state <- NULL
  if (!is.null(tour)) {
    if (!is.null(time) || !is.null(tween) || !is.null(trail)) stop("A grand tour requires static input data.", call. = FALSE)
    tour_state <- motion_tour_prepare_3d(plot, tour, frames)
    plot <- tour_state$plot
    built <- ggplot2::ggplotGrob(motion_tour_plot_3d(plot, tour_state$bases[[1L]]))
  }
  isometries <- motion_isometries_3d(isometry, times, frames)
  grDevices::dev.off(device)
  paths <- switch(output$kind, sequence = motion_sequence_paths_3d(file, frames), pages = file,
    scene = file.path(directory, sprintf("scene-%06d.pdf", seq_len(frames))),
    file.path(directory, sprintf("frame-%06d.png", seq_len(frames))))
  # Repelled labels start each frame where the previous frame of the same
  # worker left them.
  job <- list(built = built, plot = plot, time = time, values = values, limits = limits,
    isometries = isometries, tour_bases = tour_state$bases,
    keyframes = keyframes, schedule = schedule, tween = tween,
    trail = trail, trail_layers = if (!is.null(trail)) motion_trail_layers_3d(plot, time, trail),
    trail_history = new.env(parent = emptyenv()),
    cameras = cameras, base = coord$camera, backend = backend, frame = motion_frame_mode_3d(steps),
    device = output$device, per_file = output$kind != "pages", paths = paths, frames = frames,
    width = width, height = height, dpi = dpi, label_state = new.env(parent = emptyenv()),
    export = if (output$kind == "scene") list(options = options, layer_names = export_layer_names_3d(plot)),
    progress = progress && is.null(workers$cluster) && workers$count == 1L)
  if (output$kind == "scene") {
    rendered <- motion_render_frames_3d(1L, job)
    job$export$mm <- rendered[[1L]]$scene$mm
    rendered <- c(rendered, motion_render_3d(job, workers, seq_len(frames)[-1L]))
    output$write(lapply(rendered, `[[`, "scene"), file, options, fps)
  } else rendered <- motion_render_3d(job, workers)
  if (output$kind == "encode") output$encode(paths, file, width, height, fps, progress)
  complete <- TRUE
  camera <- motion_camera_table_3d(cameras, times)
  if (!is.null(time)) camera <- cbind(camera[1L], value = values, camera[-1L])
  structure(list(file = file, frame_count = as.integer(frames), fps = fps, camera = camera,
    timings = vapply(rendered, `[[`, numeric(1), "timing"), build_seconds = build_seconds,
    frame = job$frame, panel_extents = lapply(rendered, `[[`, "extents"),
    isometries = isometries, tour_bases = tour_state$bases,
    transition = if (is.null(schedule)) NULL else data.frame(from = states[schedule$from], to = states[schedule$to], fraction = schedule$fraction),
    frames = if (is.null(file) || output$kind == "sequence") paths else character()),
    class = "ggplot3d_animation")
}

motion_track_steps_3d <- function(coord, tracks, frames, times) {
  tracks <- lapply(names(tracks), function(key) {
    value <- tracks[[key]]
    if (is.null(value)) value <- coord$camera[[key]]
    if (is.function(value)) value <- value(times)
    scene_assert(is.numeric(value) && length(value) %in% c(1L, frames) &&
      all(is.finite(value)), paste(key, "must return one or frames finite values"))
    rep_len(as.double(value), frames)
  }) |> stats::setNames(names(tracks))
  start <- camera_values_3d(coord$camera)
  lapply(seq_len(frames), function(i) {
    for (key in names(tracks)) start[[key]] <- tracks[[key]][i]
    start
  })
}

motion_camera_table_3d <- function(cameras, times) {
  get <- function(key) vapply(cameras, function(camera) as.double(camera[[key]]), numeric(1))
  targets <- do.call(rbind, lapply(cameras, `[[`, "target"))
  distance <- get("distance")
  distance[!is.finite(distance)] <- NA_real_
  data.frame(time = times, azimuth = get("azimuth"), elevation = get("elevation"),
    zoom = get("zoom"), roll = get("roll"),
    projection = vapply(cameras, `[[`, character(1), "projection"), fov = get("fov"),
    distance = distance, target_x = targets[, 1L], target_y = targets[, 2L], target_z = targets[, 3L])
}

motion_frame_mode_3d <- function(steps) {
  fixed <- c("elevation", "roll", "projection", "distance", "fov", "target", "shear")
  same <- vapply(fixed, function(key) {
    all(vapply(steps, function(step) identical(step[[key]], steps[[1L]][[key]]), logical(1)))
  }, logical(1))
  if (all(same)) "azimuth" else "sphere"
}

motion_workers_3d <- function(workers) {
  if (inherits(workers, "cluster")) return(list(count = length(workers), cluster = workers))
  scene_assert(is.numeric(workers) && length(workers) == 1L && is.finite(workers) &&
    workers >= 1 && workers == trunc(workers), "workers must be a positive integer or a cluster")
  list(count = as.integer(workers), cluster = NULL)
}

motion_path_context_3d <- function(camera, params, ratio) {
  list(camera = camera, ratio = ratio,
    to_box = function(points) motion_data_to_box_3d(points, params),
    validate = function(values) motion_camera_3d(values, camera))
}

# Data coordinates to the unit box of a panel: the scale transformation, the
# panel range, then the panel frame's own mapping.
motion_data_to_box_3d <- function(points, params) {
  box <- vapply(1:3, function(k) {
    axis <- c("x", "y", "z")[k]
    value <- params[[axis]]$get_transformation()$transform(points[, k])
    range <- params$ranges[[axis]]
    (value - range[1L]) / diff(range)
  }, numeric(nrow(points)))
  box <- matrix(box, nrow(points))
  frame <- frame_of_3d(params)
  if (!isTRUE(frame$identity)) box <- frame$map(box)
  box
}

# The width-to-height ratio of the first panel's scene viewport, clamped to 1,
# for path parts whose camera depends on it. Sphere framing gives the same
# viewport for every camera, so the plot's own camera measures it.
motion_ratio_3d <- function(built, camera, backend) {
  ratio <- NULL
  function() {
    if (is.null(ratio)) {
      registry <- new.env(parent = emptyenv())
      forced <- grid::grid.force(motion_map_panels_3d(built, function(panel) {
        motion_panel_grob_3d(panel, camera, camera, backend, "sphere", registry)
      }))
      viewport <- motion_panels_3d(forced)[[1L]]$inner_viewport
      ratio <<- min(1, viewport$width_mm / viewport$height_mm)
    }
    ratio
  }
}

motion_panel_grob_3d <- function(panel, camera, base, backend, frame, registry, label_state = NULL) {
  params <- panel$panel_params
  params$camera <- motion_panel_camera_3d(camera, params$camera, base)
  params$coord_params$backend <- backend
  params$coord_params$frame <- frame
  params$coord_params$frame_registry <- registry
  params$coord_params$label_state <- label_state
  scene_panel_grob(panel$packets, params, panel$theme, panel$underlay, panel$overlay)
}

motion_frame_grob_3d <- function(job, i) {
  built <- if (!is.null(job$tour_bases)) ggplot2::ggplotGrob(motion_tour_plot_3d(job$plot, job$tour_bases[[i]])) else
    if (!is.null(job$tween)) motion_tween_build_3d(job$keyframes, job$schedule, i, job$tween) else
    if (!is.null(job$keyframes)) ggplot2::ggplot_gtable(job$keyframes[[i]]) else
    if (is.null(job$time) || i == 1L) job$built else
    ggplot2::ggplotGrob(motion_frame_plot_3d(job$plot, job$time, job$values[[i]], job$limits))
  registry <- new.env(parent = emptyenv())
  trails <- if (!is.null(job$trail)) motion_trail_history_3d(job, i)
  motion_map_panels_3d(built, function(panel) {
    if (!is.null(trails)) panel$packets <- c(panel$packets, motion_trail_packets_3d(trails, panel$panel_params, job$trail))
    if (!is.null(job$isometries)) panel <- motion_isometry_panel_3d(panel, job$isometries[[i]])
    motion_panel_grob_3d(panel, job$cameras[[i]], job$base, job$backend, job$frame, registry, job$label_state)
  })
}

motion_draw_frame_3d <- function(job, i) {
  current <- motion_frame_grob_3d(job, i)
  if (!is.null(job$export)) {
    scene <- export_capture_3d(current, 1L, job$export$options, job$export$layer_names, job$export$mm)
    return(list(extents = list(unlist(scene$viewport[c("width_mm", "height_mm")])), scene = scene))
  }
  forced <- grid::grid.force(current)
  grid::grid.draw(forced)
  list(extents = lapply(motion_panels_3d(forced), function(panel) {
    c(width_mm = panel$inner_viewport$width_mm, height_mm = panel$inner_viewport$height_mm)
  }))
}

motion_render_frames_3d <- function(indices, job) {
  lapply(indices, function(i) {
    start <- proc.time()[["elapsed"]]
    if (job$per_file) {
      device <- motion_open_device_3d(job$device, job$paths[i], job$width, job$height, job$dpi)
      on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device))
    } else if (i > indices[1L]) grid::grid.newpage()
    drawn <- motion_draw_frame_3d(job, i)
    if (job$per_file) grDevices::dev.off(device)
    timing <- proc.time()[["elapsed"]] - start
    if (job$progress) message(sprintf("Frame %d/%d: %.3f s", i, job$frames, timing))
    list(timing = timing, extents = drawn$extents, scene = drawn$scene)
  })
}

# Frames split into one contiguous run per worker; results return in frame order.
motion_render_3d <- function(job, workers, indices = seq_len(job$frames)) {
  if (!length(indices)) return(list())
  if (!job$per_file) {
    device <- motion_open_device_3d(job$device, job$paths, job$width, job$height, job$dpi)
    on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
    return(motion_render_frames_3d(indices, job))
  }
  count <- min(workers$count, length(indices))
  if (count == 1L) return(motion_render_frames_3d(indices, job))
  chunks <- unname(split(indices, cut(indices, count, labels = FALSE)))
  if (is.null(workers$cluster) && .Platform$OS.type == "unix") {
    results <- parallel::mclapply(chunks, motion_render_frames_3d, job = job,
      mc.cores = count, mc.preschedule = FALSE)
    failed <- Filter(function(result) inherits(result, "try-error"), results)
    if (length(failed)) stop(conditionMessage(attr(failed[[1L]], "condition")), call. = FALSE)
  } else {
    cluster <- workers$cluster
    if (is.null(cluster)) {
      cluster <- parallel::makePSOCKcluster(count)
      on.exit(parallel::stopCluster(cluster), add = TRUE)
    }
    results <- parallel::clusterApply(cluster, chunks, motion_render_frames_3d, job = job)
  }
  unlist(results, recursive = FALSE)
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
    framed <- ggplot2::ggproto(NULL, layer)
    framed$data <- keep(layer$data)
    framed
  })
  if (is.null(limits)) return(plot)
  plot$scales <- limits$scales$clone()
  mapping <- lapply(intersect(c("x", "y", "z"), names(limits$blank)), as.name)
  names(mapping) <- intersect(c("x", "y", "z"), names(limits$blank))
  plot + ggplot2::geom_blank(data = limits$blank, mapping = do.call(ggplot2::aes, mapping), inherit.aes = FALSE)
}

motion_export_options_3d <- function(...) {
  allowed <- c("furniture", "lines", "points", "size")
  dots <- list(...)
  scene_assert(!length(dots) || (!is.null(names(dots)) && all(names(dots) %in% allowed)),
    paste("Further arguments for glTF output must be named", paste(allowed, collapse = ", ")))
  defaults <- lapply(formals(export_3d)[allowed], eval)
  args <- utils::modifyList(defaults, dots)
  export_options_3d(args$furniture, match.arg(args$lines, defaults$lines),
    match.arg(args$points, defaults$points), args$size)
}

motion_require_3d <- function(package) {
  rlang::check_installed(package, reason = "to use this output.")
}

motion_dimensions_3d <- function(width, height, dpi) {
  scene_assert(all(vapply(list(width, height), function(x) is.numeric(x) &&
    length(x) == 1L && is.finite(x) && x >= 1 && x == trunc(x), logical(1))),
    "width and height must be positive integer pixels")
  scene_assert(is.numeric(dpi) && length(dpi) == 1L && is.finite(dpi) && dpi > 0,
    "dpi must be a positive finite number")
}

motion_plot_coord_3d <- function(plot) {
  scene_assert(inherits(plot, "ggplot") && inherits(plot$coordinates, "Coord3D"),
    "plot must be a ggplot with coord_3d()")
  plot$coordinates
}

# Camera of a frame: the coord's camera with the frame's values.
motion_camera_3d <- function(values, base) {
  camera <- unclass(base)
  for (key in names(values)) camera[[key]] <- values[[key]]
  for (key in c("distance", "near", "far")) {
    if (is.na(camera[[key]])) camera[key] <- list(NULL)
  }
  do.call(new_camera_3d, camera)
}

# A panel with its own view moves with the animated camera and keeps its
# offset from the coord's first view: azimuth, elevation and roll differences
# add, the zoom ratio multiplies. A panel whose projection (with its distance,
# field of view, clipping planes and shear), target or vertical axis is set by
# its own view keeps it; where it shares the coord's value, it follows the
# animated camera, as a camera path moves these too.
motion_panel_camera_3d <- function(camera, panel, base) {
  out <- panel
  out$azimuth <- panel$azimuth + camera$azimuth - base$azimuth
  out$elevation <- max(-90, min(90, panel$elevation + camera$elevation - base$elevation))
  out$roll <- panel$roll + camera$roll - base$roll
  out$zoom <- panel$zoom * camera$zoom / base$zoom
  groups <- list(c("projection", "distance", "fov", "near", "far", "shear"), "target", "up")
  for (keys in groups) {
    shared <- all(vapply(keys, function(key) identical(panel[[key]], base[[key]]), logical(1)))
    if (shared) out[keys] <- unclass(camera)[keys]
  }
  out
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


motion_isometries_3d <- function(isometry, times, frames) {
  if (is.null(isometry)) return(NULL)
  if (is.function(isometry)) isometry <- isometry(times)
  if (is.matrix(isometry)) isometry <- rep(list(isometry), frames)
  if (!is.list(isometry) || length(isometry) != frames ||
      !all(vapply(isometry, function(x) is.matrix(x) && is.numeric(x) && all(is.finite(x)), logical(1)))) {
    stop("isometry must supply one finite canonical matrix per frame.", call. = FALSE)
  }
  isometry
}

motion_isometry_panel_3d <- function(panel, isometry) {
  context <- panel$panel_params$space_context
  if (is.null(context)) stop("Isometry animation requires a non-default coordinate space.", call. = FALSE)
  if (isTRUE(context$linear_action)) return(motion_action_panel_3d(panel, isometry))
  panel$panel_params$space_context$isometry <- isometry
  panel$packets <- lapply(panel$packets, function(packet) {
    recipe <- attr(packet, "draw_recipe")
    if (is.null(recipe) || !identical(recipe$panel_params$space_context$space, context$space)) return(packet)
    recipe$panel_params$space_context$isometry <- isometry
    do.call(emit_scene_3d, recipe)
  })
  panel
}


# A frame whose isometry acts linearly on the display model changes only the
# camera pre-transform and the observer-dependent styles; the geometry is not
# emitted again.
motion_action_panel_3d <- function(panel, isometry) {
  context <- panel$panel_params$space_context
  context$action <- context$space$projective_action(isometry, context$model)
  panel$panel_params$space_context <- context
  panel$panel_params$camera <- space_action_camera_3d(panel$panel_params$camera, context)
  panel$packets <- lapply(panel$packets, function(packet) {
    unstyled <- attr(packet, "space_unstyled")
    if (is.null(unstyled)) return(packet)
    styled <- context$space$style_packet(unstyled, context)
    for (name in setdiff(names(attributes(packet)), names(attributes(styled)))) attr(styled, name) <- attr(packet, name)
    styled
  })
  panel
}

motion_tour_plot_3d <- function(plot, basis) {
  plot@layers <- lapply(plot@layers, function(layer) {
    if (!inherits(layer$stat, "StatProject3D")) return(layer)
    stat_params <- layer$stat_params
    geom_params <- layer$geom_params
    stat_params$basis <- basis
    geom_params$basis <- basis
    ggplot2::ggproto(NULL, layer, stat_params = stat_params, geom_params = geom_params)
  })
  plot
}

motion_tour_prepare_3d <- function(plot, tour, frames) {
  index <- which(vapply(plot@layers, function(layer) inherits(layer$stat, "StatProject3D"), logical(1)))
  if (!length(index)) stop("A tour requires a stat_project_3d() layer.", call. = FALSE)
  dimensions <- lapply(plot@layers[index], function(layer) layer$stat_params$dimensions)
  p <- length(dimensions[[1L]])
  if (any(lengths(dimensions) != p) || any(vapply(plot@layers[index], function(layer) layer$stat_params$projection == "stereographic", logical(1)))) {
    stop("Tour layers require the same input dimension and orthographic or finite perspective projection; stereographic pole crossings require #16.", call. = FALSE)
  }
  built <- ggplot2::ggplot_build(plot)
  for (axis in c("x", "y", "z")) {
    scale <- built$plot@scales$get_scales(axis)
    if (!is.null(scale) && !identical(scale$get_transformation()$name, "identity")) {
      stop("Use linear position scales for tours; transform input dimensions before plotting.", call. = FALSE)
    }
  }
  bases <- tour_basis_frames_3d(tour, p, frames)
  radius <- max(vapply(index, function(i) {
    X <- as.matrix(built$data[[i]][paste0(".nd", seq_len(p))])
    params <- plot@layers[[i]]$stat_params
    if (params$projection == "orthographic") return(max(sqrt(rowSums(X^2))))
    max(vapply(bases, function(basis) max(sqrt(rowSums(project_nd_3d(X, basis, params$projection, params$eye)^2))), numeric(1)))
  }, numeric(1)))
  for (i in setdiff(seq_along(built$data), index)) {
    d <- built$data[[i]]
    values <- unlist(d[intersect(names(d), c("x", "y", "z", "xend", "yend", "zend"))], use.names = FALSE)
    if (any(is.finite(values))) radius <- max(radius, abs(values[is.finite(values)]))
  }
  if (!is.finite(radius)) stop("Tour coordinates must be finite.", call. = FALSE)
  if (radius == 0) radius <- 1
  parent_coord <- plot@coordinates
  coord <- ggplot2::ggproto(NULL, parent_coord)
  coord$limits <- lapply(coord$limits, function(limits) limits %or3d% c(-radius, radius))
  plot@coordinates <- coord
  list(plot = plot, bases = bases)
}
