#' Lighting for a 3D scene
#' @param direction Direction from a surface toward the light.
#' @param space Coordinate space, camera or data.
#' @param ambient,diffuse Nonnegative light contributions with sum at most one.
#' @param mode Colour operation, multiply or tint.
#' @return A lighting configuration for coord_3d().
#' @examples
#' light_3d(direction = c(1, 1, 2), space = "camera")
#' @export
light_3d <- function(direction = c(-1, 1, 2), space = "camera", ambient = 0.35, diffuse = 0.65, mode = "multiply") {
  stopifnot(is.numeric(direction), length(direction) == 3L, all(is.finite(direction)), sum(direction^2) > 0,
            length(ambient) == 1L, length(diffuse) == 1L, is.finite(ambient), is.finite(diffuse),
            ambient >= 0, diffuse >= 0, ambient + diffuse <= 1)
  list(direction = direction / sqrt(sum(direction^2)), space = match.arg(space, c("camera", "data")),
       ambient = ambient, diffuse = diffuse, mode = match.arg(mode, c("multiply", "tint")))
}

#' Select a 3D rendering backend
#' @param type Backend: `"vector"`, `"raster"`, `"hybrid"` or `"pathtrace"`. A hybrid panel
#'   rasterises the layers created with `raster = TRUE` (an argument of every
#'   `geom_*_3d()` layer and of [layer_3d()]) into one image at `dpi` and draws
#'   the other layers, axes, panes and text as vector graphics. Vector
#'   geometry behind the image's opaque coverage is clipped at its pixels, so
#'   lines on or behind a rasterised surface stay exact where they are seen.
#' @param dpi Physical raster resolution.
#' @param antialias Positive integer supersampling factor.
#' @param samples Path-tracing samples per pixel added by each render.
#' @param bounces Maximum diffuse surface interactions per path.
#' @param seed Nonnegative integer seed for pixel and sample indexed randomness.
#' @param aperture Thin-lens radius in aspect-scaled model units; zero is a pinhole.
#' @param focus Focal-plane distance along the camera forward axis, in model
#'   units. NULL focuses on the camera target.
#' @param light_radius Angular radius of the distant light in degrees.
#' @param progressive Accumulate samples across redraws of the same scene.
#'   Changes to geometry, camera, lighting or image dimensions reset accumulation.
#' @param batch_size Samples per native batch; accumulation is independent of
#'   batch size and worker count. R can interrupt between batches.
#' @param blend Colour space in which the raster backend composites
#'   translucent paint and averages antialiasing samples: `"linear"` light,
#'   encoded to sRGB after averaging, or `"srgb"`, which matches the
#'   compositing of vector graphics devices.
#' @details The raster backend renders 64-pixel tiles in parallel. The number
#'   of worker threads is the option `ggplot3d.threads`; the default `0` uses
#'   one thread per hardware thread, at most two when `_R_CHECK_LIMIT_CORES_`
#'   is set. The image does not depend on the thread count.
#'
#' The native path tracer uses diffuse materials, a bounding-volume hierarchy,
#' cosine-weighted reflection sampling and Russian roulette. The ambient light
#' is a uniform environment; the diffuse light is a sampled distant disk.
#' Points and paths use the solid conversion of [export_3d()], and SDF layers
#' use the shared sphere tracer. Furniture and text remain vector graphics.
#' @references Pharr, Jakob and Humphreys (2023). Physically Based Rendering,
#'   fourth edition. <https://pbr-book.org/4ed/Light_Transport_I_Surface_Reflection/Path_Tracing>
#' @return A backend configuration for coord_3d().
#' @examples
#' backend_3d("vector")
#' backend_3d("raster", dpi = 144, antialias = 2)
#' backend_3d("raster", blend = "srgb")
#' backend_3d("pathtrace", samples = 8, seed = 1)
#' d <- expand.grid(x = seq(-1, 1, length.out = 40), y = seq(-1, 1, length.out = 40))
#' d$z <- sin(pi * d$x) * cos(pi * d$y)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(fill = "grey80", colour = NA, raster = TRUE) +
#'   geom_contour_3d(colour = "black") +
#'   coord_3d(backend = backend_3d("hybrid", dpi = 150))
#' @export
backend_3d <- function(type = "vector", dpi = 144, antialias = 2L, blend = "linear",
                       samples = 64L, bounces = 8L, seed = 1L, aperture = 0,
                       focus = NULL, light_radius = 5, progressive = FALSE, batch_size = 8L) {
  stopifnot(length(dpi) == 1L, is.finite(dpi), dpi > 0, length(antialias) == 1L,
            is.finite(antialias), antialias >= 1, antialias == trunc(antialias))
  result <- list(type = match.arg(type, c("vector", "raster", "hybrid", "pathtrace")), dpi = dpi, antialias = as.integer(antialias),
       blend = match.arg(blend, c("linear", "srgb")))
  if (result$type == "pathtrace") result <- c(result,
    pathtrace_settings_3d(samples, bounces, seed, aperture, focus, light_radius, progressive, batch_size))
  result
}

#' Exact three-dimensional panel coordinates
#'
#' Set `z_scales = "free"` here to train a separate z range per facet.
#' ggplot2 facets control x and y scales and have no z scale.
#' @param projection `"orthographic"`, `"perspective"` or `"oblique"`. An
#'   oblique projection is parallel with a sight ray that is not perpendicular
#'   to the image: depth recedes on the page along `oblique`.
#' @param oblique Receding direction of an oblique projection: `angle` in
#'   degrees counterclockwise from the page's right, and `ratio`, the drawn
#'   length of one unit of depth.
#' @param up Vertical axis, `"x"`, `"y"` or `"z"`. Azimuth turns about it and
#'   elevation rises toward it.
#' @param azimuth,elevation,roll Camera angles in degrees.
#' @param distance,fov,zoom Camera distance, field of view and magnification.
#'   Each camera argument from `azimuth` to `zoom` takes one value, shared by
#'   all panels, or one value per panel in layout order, so facets can show
#'   the scene from different views.
#' @param target Target fractions within the expanded box.
#' @param near,far Camera clipping distances.
#' @param aspect Three positive model side lengths, or "data".
#' @param frame Fit the current view, all azimuths at this elevation, or a sphere.
#' @param xlim,ylim,zlim Coordinate zoom limits in original units.
#' @param expand Logical scalar or named x, y, z logical vector.
#' @param clip Rectangular viewport clipping, on or off.
#' @param clip_data Whether to clip to the data box.
#' @param clip_planes A list of [clip_plane_3d()] planes that cut every layer
#'   of the panel. A layer's own `clip_planes` argument replaces this list for
#'   that layer.
#' @param axes Axes to draw.
#' @param panes Cube faces to draw: `"back"` (the rear pane of each pair of
#'   walls), `"floor"`, `"front"` (the panes facing the viewer that are not
#'   drawn otherwise, styled by the theme element `panel.pane.front` and drawn
#'   over the data without grid lines), or pane names `"xmin"` to `"zmax"`.
#' @param box Box edges, back, all or none.
#' @param axis_lines Where axis lines and their ticks and labels sit:
#'   `"box"` on an edge of the box, `"origin"` through the point `origin`, or
#'   `"none"`, which keeps ticks and labels on the box edges and draws no axis
#'   lines. A guide with an explicit position (see [guide_axis_3d()]) keeps
#'   it. Origin axes, their ticks, tick labels and titles are part of the
#'   scene, so data in front of them hide them; the label at the point where
#'   origin axes cross is left out. Origin axis lines use the theme elements
#'   `axis.line.origin.x`, `.y` and `.z`, which inherit from
#'   `axis.line.origin`.
#' @param origin Named numeric vector with entries `x`, `y` and `z` in data
#'   units, the point through which origin axes pass. Missing entries are 0. A
#'   value outside the panel range is moved to the nearest end of the range.
#' @param z_scales Fixed or free z scales across panels.
#' @param crs Target spatial coordinate reference system.
#' @param light Lighting configuration or NULL.
#' @param backend Backend configuration.
#' @param stereo Stereo output from [stereo_3d()], or NULL for one view.
#' @param transform Optional coordinate transformations applied after
#'   statistics, as a named list with entries `x`, `y` or `z`, each a
#'   transformation name such as `"log10"` or a scales transformation. Lines
#'   and faces are subdivided so their images bend with the transformation.
#' @param default Whether this is the default coordinate system.
#' @section Theme elements:
#' Besides the ggplot2 elements, panels read these theme elements:
#' \itemize{
#'   \item `panel.pane`, `panel.pane.x`, `panel.pane.y`, `panel.pane.z`: pane
#'     fills, by the axis normal to the pane; `panel.pane.front` styles the
#'     panes selected with `panes = "front"`.
#'   \item `panel.grid.major.x.xy`, `panel.grid.minor.z.yz` and so on: the grid
#'     lines of one axis on the panes of one plane. Each inherits from the
#'     grid element of its axis (`panel.grid.major.x.xy` from
#'     `panel.grid.major.x`), so `element_blank()` removes those lines from
#'     that plane only. In the default view the floor is the `xy` plane.
#'   \item `panel.zero.x`, `panel.zero.y`, `panel.zero.z`: a plane through the
#'     panel at transformed value 0 of that axis, when 0 lies inside the range.
#'     They are blank by default; setting one to an [ggplot2::element_rect()]
#'     draws it, filled from `panel.zero`. Zero planes are part of the scene,
#'     so they hide data behind them.
#'   \item `panel.zero.line.x`, `.y`, `.z`, inheriting from `panel.zero.line`
#'     (blank by default): the line at transformed value 0 on each drawn pane.
#'   \item `panel.box.back`, `panel.box.front`: box edges.
#'   \item `axis.line.origin.x`, `.y`, `.z`: origin axis lines.
#'   \item `panel.margin.3d`: the margin around the projected box.
#' }
#' @return A Coord3D ggproto object.
#' @examples
#' d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_point_3d(size = 3) + coord_3d(azimuth = -45, elevation = 25)
#' d$site <- c("a", "a", "b")
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
#'   ggplot2::facet_wrap(~site) + coord_3d(z_scales = "free")
#' views <- rbind(transform(d, view = "front"), transform(d, view = "side"))
#' ggplot2::ggplot(views, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
#'   ggplot2::facet_wrap(~view) + coord_3d(azimuth = c(-60, 30))
#' centred <- data.frame(x = c(-2, 1, 2), y = c(1, -2, 2), z = c(-1, 2, 1))
#' ggplot2::ggplot(centred, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 3) +
#'   coord_3d(axis_lines = "origin", panes = "floor") +
#'   ggplot2::theme(panel.zero.z = ggplot2::element_rect(),
#'     panel.zero.line = ggplot2::element_line(colour = "grey30"))
#' ggplot2::ggplot(centred, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 3) +
#'   coord_3d(panes = c("back", "floor", "front")) +
#'   ggplot2::theme(panel.grid.major.z.xz = ggplot2::element_blank(),
#'     panel.pane.front = ggplot2::element_rect(fill = "#4682B41A"))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
#'   coord_3d(clip_planes = list(clip_plane_3d(c(0, 0, -1), c(z = 2.5))))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
#'   coord_3d(stereo = stereo_3d("cross"))
#' @export
coord_3d <- function(projection = "orthographic", azimuth = -60, elevation = 30, roll = 0,
                     oblique = c(angle = 45, ratio = 0.5), up = "z",
                     distance = NULL, fov = 35, zoom = 1, target = c(0.5, 0.5, 0.5), near = NULL, far = NULL,
                     aspect = c(1, 1, 1), xlim = NULL, ylim = NULL, zlim = NULL,
                     expand = TRUE, clip = "on", clip_data = TRUE, axes = c("x", "y", "z"),
                     panes = c("back", "floor"), box = "back", z_scales = "fixed", crs = NULL,
                     light = light_3d(), backend = backend_3d(), default = FALSE, frame = "view",
                     transform = NULL, axis_lines = "box", origin = c(x = 0, y = 0, z = 0),
                     clip_planes = list(), stereo = NULL) {
  clip <- match.arg(clip, c("on", "off"))
  clip_planes <- check_clip_planes_3d(clip_planes)
  if (!is.logical(expand) || anyNA(expand) || !length(expand) ||
      !(length(expand) == 1L || (length(expand) == 3L && setequal(names(expand), c("x", "y", "z"))))) stop("expand must be logical scalar or named x/y/z vector.", call. = FALSE)
  expand_axes <- if (length(expand) == 1L) structure(rep(expand, 3L), names = c("x", "y", "z")) else expand[c("x", "y", "z")]
  if (!all(axes %in% c("x", "y", "z"))) stop("Unknown 3D axis.", call. = FALSE)
  if (!all(panes %in% c("back", "floor", "front", "xmin", "xmax", "ymin", "ymax", "zmin", "zmax"))) stop("Unknown cube pane.", call. = FALSE)
  if (!is.null(stereo) && !inherits(stereo, "ggplot3d_stereo")) stop("stereo must be NULL or a stereo_3d() configuration.", call. = FALSE)
  views <- list(azimuth = azimuth, elevation = elevation, roll = roll, distance = distance, fov = fov, zoom = zoom)
  sizes <- lengths(views)
  count <- max(1L, sizes)
  if (any(sizes > 1L & sizes != count)) stop("Camera arguments must have one value or the same number of values, one per panel.", call. = FALSE)
  base <- list(projection = projection, azimuth = azimuth, elevation = elevation, roll = roll, distance = distance,
    fov = fov, zoom = zoom, target = target, oblique = oblique, up = up,
    aspect = if (identical(aspect, "data")) c(1, 1, 1) else aspect,
    near = near, far = far, clip_data = clip_data, clip = clip == "on")
  panel_args <- lapply(seq_len(count), function(i) {
    view <- lapply(views, function(v) if (length(v) > 1L) v[[i]] else v)
    camera_args_update_3d(base, view)
  })
  cameras <- lapply(panel_args, camera_from_args_3d)
  ggplot2::ggproto(NULL, Coord3D, camera = cameras[[1L]], cameras = cameras, camera_args = panel_args[[1L]], model_aspect = aspect, frame = frame_box_3d(transform),
    limits = list(x = xlim, y = ylim, z = zlim), expand_axes = expand_axes,
    clip = clip, default = default, z_scales = match.arg(z_scales, c("fixed", "free")),
    config = list(axes = unique(axes), panes = unique(panes), box = match.arg(box, c("back", "all", "none")),
                  axis_lines = match.arg(axis_lines, c("box", "origin", "none")), origin = check_origin_3d(origin),
                  light = light, backend = backend, crs = crs, clip_planes = clip_planes, stereo = stereo,
                  frame = match.arg(frame, c("view", "azimuth", "sphere")), frame_registry = new.env(parent = emptyenv())))
}

check_origin_3d <- function(origin) {
  if (!(is.numeric(origin) && length(origin) && all(is.finite(origin)) && !is.null(names(origin)) &&
      all(names(origin) %in% c("x", "y", "z")) && !anyDuplicated(names(origin)))) {
    stop("origin must be a named numeric vector with entries x, y or z.", call. = FALSE)
  }
  out <- c(x = 0, y = 0, z = 0)
  out[names(origin)] <- origin
  out
}

oblique_shear_3d <- function(projection, oblique) {
  if (projection != "oblique") return(c(0, 0))
  if (!(is.numeric(oblique) && length(oblique) == 2L && all(is.finite(oblique)))) stop("oblique must be c(angle = , ratio = ).", call. = FALSE)
  if (!is.null(names(oblique))) {
    if (!setequal(names(oblique), c("angle", "ratio"))) stop("oblique must be named angle and ratio.", call. = FALSE)
    oblique <- oblique[c("angle", "ratio")]
  }
  if (oblique[2L] <= 0) stop("The oblique ratio must be positive.", call. = FALSE)
  angle <- oblique[[1L]] * pi / 180
  unname(oblique[[2L]] * c(cos(angle), sin(angle)))
}

# Camera from coord_3d() camera arguments: the projection, angles, framing
# values, oblique receding direction and vertical axis by name.
camera_from_args_3d <- function(args) {
  projection <- match.arg(args$projection, c("orthographic", "perspective", "oblique"))
  up <- match(match.arg(args$up, c("x", "y", "z")), c("x", "y", "z"))
  new_camera_3d(projection, args$azimuth, args$elevation, args$roll, args$distance, args$fov, args$zoom,
    args$target, args$aspect, args$near, args$far, args$clip_data, args$clip, up = up,
    shear = oblique_shear_3d(projection, args$oblique))
}

camera_view_names_3d <- c("projection", "azimuth", "elevation", "roll", "distance", "fov", "zoom", "target", "oblique", "up")

# Camera arguments with the entries of a view replacing their base values; a
# NULL entry, such as distance = NULL, is kept as an automatic value.
camera_args_update_3d <- function(args, view) {
  unknown <- setdiff(names(view), camera_view_names_3d)
  if (length(unknown) || (length(view) && is.null(names(view))))
    stop("A camera view takes the camera_3d() arguments ", paste(camera_view_names_3d, collapse = ", "), ".", call. = FALSE)
  for (name in names(view)) args[name] <- list(view[[name]])
  args
}

camera_presets_3d <- function() {
  # Dimetric with x foreshortened to half of y and z: sin(azimuth)^2 = 1/8,
  # cos(elevation)^2 = 8/9.
  list(
    default = list(projection = "orthographic", azimuth = -60, elevation = 30, roll = 0),
    isometric = list(projection = "orthographic", azimuth = -45, elevation = atan(1 / sqrt(2)) * 180 / pi, roll = 0),
    dimetric = list(projection = "orthographic", azimuth = -asin(sqrt(1 / 8)) * 180 / pi, elevation = acos(sqrt(8 / 9)) * 180 / pi, roll = 0),
    trimetric = list(projection = "orthographic", azimuth = -30, elevation = 20, roll = 0),
    top = list(projection = "orthographic", azimuth = -90, elevation = 90, roll = 0),
    bottom = list(projection = "orthographic", azimuth = -90, elevation = -90, roll = 0),
    front = list(projection = "orthographic", azimuth = -90, elevation = 0, roll = 0),
    rear = list(projection = "orthographic", azimuth = 90, elevation = 0, roll = 0),
    left = list(projection = "orthographic", azimuth = 180, elevation = 0, roll = 0),
    right = list(projection = "orthographic", azimuth = 0, elevation = 0, roll = 0),
    cabinet = list(projection = "oblique", azimuth = -90, elevation = 0, roll = 0, oblique = c(angle = 45, ratio = 0.5)),
    cavalier = list(projection = "oblique", azimuth = -90, elevation = 0, roll = 0, oblique = c(angle = 45, ratio = 1)),
    military = list(projection = "oblique", azimuth = -45, elevation = 90, roll = 0, oblique = c(angle = -90, ratio = 1))
  )
}

#' Named camera views
#'
#' Returns camera arguments for [coord_3d()], so a preset and further
#' arguments combine with `do.call()`. Explicit non-NULL arguments override
#' the preset.
#' @param preset One of `"default"`, `"isometric"`, `"dimetric"` (x drawn at
#'   half the length of y and z), `"trimetric"`, the six principal views
#'   `"top"`, `"bottom"`, `"front"`, `"rear"`, `"left"` and `"right"` (each
#'   looking along one axis, as placed by [facet_views()]), and the oblique views `"cabinet"` (depth at 45 degrees, half length),
#'   `"cavalier"` (depth at 45 degrees, full length) and `"military"` (the
#'   floor plan turned 45 degrees, heights drawn straight up at full length).
#' @param projection,azimuth,elevation,roll,distance,fov,zoom,target,oblique,up
#'   Overrides of the preset values; see [coord_3d()].
#' @return A named list of [coord_3d()] arguments.
#' @examples
#' d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d()
#' p + do.call(coord_3d, camera_3d("isometric"))
#' p + do.call(coord_3d, c(camera_3d("cabinet"), list(panes = "floor")))
#' @export
camera_3d <- function(preset = c("default", "isometric", "dimetric", "trimetric", "top", "bottom", "front",
                                 "rear", "left", "right", "cabinet", "cavalier", "military"),
                      projection = NULL, azimuth = NULL, elevation = NULL, roll = NULL,
                      distance = NULL, fov = NULL, zoom = NULL, target = NULL, oblique = NULL, up = NULL) {
  values <- camera_presets_3d()[[match.arg(preset)]]
  overrides <- list(projection = projection, azimuth = azimuth, elevation = elevation, roll = roll,
    distance = distance, fov = fov, zoom = zoom, target = target, oblique = oblique, up = up)
  for (name in names(overrides)) if (!is.null(overrides[[name]])) values[[name]] <- overrides[[name]]
  values
}

Coord3D <- ggplot2::ggproto("Coord3D", ggplot2::Coord,
  frame = NULL,
  cameras = NULL,
  # Camera of the panel in layout row `index` of `count` panels.
  panel_camera = function(self, index, count) {
    cameras <- self$cameras %or3d% list(self$camera)
    if (length(cameras) == 1L) return(self$camera)
    if (length(cameras) != count) stop("coord_3d() has ", length(cameras), " camera views but the plot has ", count, " panels.", call. = FALSE)
    cameras[[index]]
  },
  # Camera of a panel showing `view`, a list of camera_3d() arguments that
  # replace the coord's own.
  view_camera = function(self, view) {
    if (length(self$cameras) > 1L) stop("Give camera views either to facet_views() or as camera argument vectors of coord_3d(), not both.", call. = FALSE)
    camera_from_args_3d(camera_args_update_3d(self$camera_args, view))
  },
  is_free = function() TRUE,
  is_linear = function() FALSE,
  # A side-by-side stereo panel holds two square eye images.
  aspect = function(self, ranges) if (stereo_side_by_side_3d(self$config$stereo)) 0.5 else 1,
  setup_params = function(self, data) list(),
  setup_layout = function(self, layout, params) {
    # Views of one scene (facet_views()) share its z scale.
    free <- self$z_scales == "free" && is.null(layout$VIEW)
    layout$SCALE_Z <- if (free) seq_len(nrow(layout)) else rep.int(1L, nrow(layout))
    triples <- paste(layout$SCALE_X, layout$SCALE_Y, layout$SCALE_Z, sep = ":")
    layout$COORD <- match(triples, unique(triples))
    layout
  },
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    c(self$axis_view(scale_x, "x"), self$axis_view(scale_y, "y"))
  },
  axis_view = function(self, scale, axis) {
    (self$frame %or3d% FrameBox)$axis_view(scale, self$limits[[axis]], self$expand_axes[[axis]], axis)
  },
  range = function(panel_params) panel_params$ranges,
  backtransform_range = function(panel_params) {
    structure(lapply(c("x", "y", "z"), function(a) panel_params[[a]]$get_transformation()$inverse(panel_params$ranges[[a]])), names = c("x", "y", "z"))
  },
  transform = function(data, panel_params) project_positions_3d(data, panel_params),
  distance = function(x, y, panel_params, z = NULL) {
    squared <- diff(x / diff(panel_params$ranges$x))^2 + diff(y / diff(panel_params$ranges$y))^2
    if (!is.null(z)) squared <- squared + diff(z / diff(panel_params$ranges$z))^2
    sqrt(squared)
  },
  render_bg = function(panel_params, theme) ggplot2::zeroGrob(),
  render_fg = function(panel_params, theme) ggplot2::zeroGrob(),
  render_axis_h = function(panel_params, theme) list(top = ggplot2::zeroGrob(), bottom = ggplot2::zeroGrob()),
  render_axis_v = function(panel_params, theme) list(left = ggplot2::zeroGrob(), right = ggplot2::zeroGrob()),
  labels = function(labels, panel_params) list(x = NULL, y = NULL),
  draw_panel = function(self, panel, params, theme) {
    collected <- collect_scene_layers(panel, params, theme)
    scene_panel_grob(collected$packets, params, theme, collected$underlay, collected$overlay)
  }
)

normalise_positions_3d <- function(data, panel_params) {
  for (axis in c("x", "y", "z")) {
    family <- if (axis == "z") z_aesthetics else panel_params[[axis]]$aesthetics
    range <- panel_params$ranges[[axis]]
    normal <- paste0("n", axis)
    if (normal %in% names(data)) data[[normal]] <- data[[normal]] * diff(range)
    for (column in intersect(family, names(data))) {
      value <- as.numeric(data[[column]])
      value[value == -Inf] <- range[1L]
      value[value == Inf] <- range[2L]
      data[[column]] <- (value - range[1L]) / diff(range)
    }
  }
  data
}

project_positions_3d <- function(data, panel_params) {
  check_3d_build(!is.null(panel_params$camera) && !is.null(panel_params$ranges$z))
  if (is.null(data$z)) data$z <- rep(0, nrow(data))
  normal <- normalise_positions_3d(data, panel_params)
  coordinates <- normal[c("x", "y", "z")]
  frame <- frame_of_3d(panel_params)
  if (!isTRUE(frame$identity)) {
    mapped <- frame$map(as.matrix(coordinates))
    coordinates[] <- lapply(seq_len(3L), function(k) mapped[, k])
  }
  has_end <- any(c("xend", "yend", "zend") %in% names(normal))
  if (has_end) {
    end <- coordinates
    for (axis in c("x", "y", "z")) if (!is.null(normal[[paste0(axis, "end")]])) end[[axis]] <- normal[[paste0(axis, "end")]]
    coordinates <- rbind(coordinates, end)
  }
  valid <- which(Reduce(`&`, lapply(coordinates, is.finite)))
  projected <- matrix(NA_real_, nrow(coordinates), 6L,
                      dimnames = list(NULL, c("x_npc", "y_npc", "x_camera", "y_camera", "depth", "clip_w")))
  if (length(valid)) {
    n <- length(valid)
    packet <- new_scene_packet(vertices = lapply(coordinates[valid, , drop = FALSE], as.double),
      corners = list(vertex = seq_len(n), source_row = as.integer(valid)),
      markers = list(offset = seq_len(n), footprint_id = rep.int(1L, n), source_row = as.integer(valid)),
      footprints = list(kind = 1L, offset = 1L, count = 0L, radius_x_mm = 0, radius_y_mm = 0, angle = 0),
      styles = data.frame(style_id = 1L), panel_id = panel_params$panel_id)
    viewport <- emit_viewport_3d(panel_params)
    compiled <- scene_compile(packet, panel_params$camera, viewport)
    f <- compiled$fragments
    if (length(f$offset)) {
      for (name in colnames(projected)) projected[f$source_row, name] <- compiled$vertices[[name]][f$offset]
    }
  }
  start <- seq_len(nrow(data))
  out <- data
  out$x <- projected[start, "x_npc"]; out$y <- projected[start, "y_npc"]
  for (axis in c("x", "y", "z")) out[[paste0(".", axis, "_model")]] <- normal[[axis]]
  for (name in c("x_camera", "y_camera", "depth", "clip_w")) out[[paste0(".", name)]] <- projected[start, name]
  out$.z_camera <- out$.depth
  out$.visible <- is.finite(out$x) & is.finite(out$y)
  if (has_end) {
    out$xend <- projected[start + nrow(data), "x_npc"]
    out$yend <- projected[start + nrow(data), "y_npc"]
  }
  out
}

minimal_scene_panel_3d <- function(packets, params, viewport = new_scene_viewport(100, 100)) {
  if (!identical(params$coord_params$backend$type, "vector")) stop("The minimal integration renderer requires the vector backend.", call. = FALSE)
  scene <- scene_compile(merge_scene_packets(packets, params$panel_id), params$camera, viewport)
  fragments <- scene$fragments
  grobs <- lapply(scene$draw_order, function(i) {
    indices <- seq.int(fragments$offset[i], length.out = fragments$count[i])
    style <- scene$styles[match(fragments$style_id[i], scene$styles$style_id), , drop = FALSE]
    x <- scene$vertices$x_npc[indices]; y <- scene$vertices$y_npc[indices]
    gp <- grid::gpar(col = style$colour, fill = style$fill, alpha = fragments$opacity[i])
    switch(fragments$kind[i], grid::polygonGrob(x, y, gp = gp), grid::linesGrob(x, y, gp = gp),
           grid::pointsGrob(x, y, pch = style$shape, size = grid::unit(style$size, "mm"), default.units = "npc", gp = gp),
           grid::textGrob(style$label, x, y, gp = gp))
  })
  grid::gTree(children = do.call(grid::gList, grobs), scene = scene)
}
