#' Lighting for a 3D scene
#' @param direction Direction from a surface toward the light.
#' @param space Coordinate space, camera or data.
#' @param ambient,diffuse Nonnegative light contributions with sum at most one.
#' @param mode Colour operation, multiply or tint.
#' @export
light_3d <- function(direction = c(-1, 1, 2), space = "camera", ambient = 0.35, diffuse = 0.65, mode = "multiply") {
  stopifnot(is.numeric(direction), length(direction) == 3L, all(is.finite(direction)), sum(direction^2) > 0,
            length(ambient) == 1L, length(diffuse) == 1L, is.finite(ambient), is.finite(diffuse),
            ambient >= 0, diffuse >= 0, ambient + diffuse <= 1)
  list(direction = direction / sqrt(sum(direction^2)), space = match.arg(space, c("camera", "data")),
       ambient = ambient, diffuse = diffuse, mode = match.arg(mode, c("multiply", "tint")))
}

#' Select a 3D rendering backend
#' @param type Backend, vector or raster.
#' @param dpi Physical raster resolution.
#' @param antialias Positive integer supersampling factor.
#' @export
backend_3d <- function(type = "vector", dpi = 144, antialias = 2L) {
  stopifnot(length(dpi) == 1L, is.finite(dpi), dpi > 0, length(antialias) == 1L,
            is.finite(antialias), antialias >= 1, antialias == trunc(antialias))
  list(type = match.arg(type, c("vector", "raster")), dpi = dpi, antialias = as.integer(antialias))
}

#' Exact three-dimensional panel coordinates
#' @param projection Orthographic or perspective projection.
#' @param azimuth,elevation,roll Camera angles in degrees.
#' @param distance,fov,zoom Camera distance, field of view and magnification.
#' @param target Target fractions within the expanded box.
#' @param near,far Camera clipping distances.
#' @param aspect Three positive model side lengths, or "data".
#' @param frame Fit the current view, all azimuths at this elevation, or a sphere.
#' @param xlim,ylim,zlim Coordinate zoom limits in original units.
#' @param expand Logical scalar or named x, y, z logical vector.
#' @param clip Rectangular viewport clipping, on or off.
#' @param clip_data Whether to clip to the data box.
#' @param axes Axes to draw.
#' @param panes Cube faces to draw.
#' @param box Box edges, back, all or none.
#' @param z_scales Fixed or free z scales across panels.
#' @param crs Target spatial coordinate reference system.
#' @param light Lighting configuration or NULL.
#' @param backend Backend configuration.
#' @param default Whether this is the default coordinate system.
#' @return A Coord3D ggproto object.
#' @export
coord_3d <- function(projection = "orthographic", azimuth = -60, elevation = 30, roll = 0,
                     distance = NULL, fov = 35, zoom = 1, target = c(0.5, 0.5, 0.5), near = NULL, far = NULL,
                     aspect = c(1, 1, 1), xlim = NULL, ylim = NULL, zlim = NULL,
                     expand = TRUE, clip = "on", clip_data = TRUE, axes = c("x", "y", "z"),
                     panes = c("back", "floor"), box = "back", z_scales = "fixed", crs = NULL,
                     light = light_3d(), backend = backend_3d(), default = FALSE, frame = "view") {
  clip <- match.arg(clip, c("on", "off"))
  if (!is.logical(expand) || anyNA(expand) || !length(expand) ||
      !(length(expand) == 1L || (length(expand) == 3L && setequal(names(expand), c("x", "y", "z"))))) stop("expand must be logical scalar or named x/y/z vector.", call. = FALSE)
  expand_axes <- if (length(expand) == 1L) structure(rep(expand, 3L), names = c("x", "y", "z")) else expand[c("x", "y", "z")]
  if (!all(axes %in% c("x", "y", "z"))) stop("Unknown 3D axis.", call. = FALSE)
  if (!all(panes %in% c("back", "floor", "xmin", "xmax", "ymin", "ymax", "zmin", "zmax"))) stop("Unknown cube pane.", call. = FALSE)
  camera <- new_camera_3d(projection, azimuth, elevation, roll, distance, fov, zoom, target,
                          if (identical(aspect, "data")) c(1, 1, 1) else aspect,
                          near, far, clip_data, clip == "on")
  ggplot2::ggproto(NULL, Coord3D, camera = camera, model_aspect = aspect,
    limits = list(x = xlim, y = ylim, z = zlim), expand_axes = expand_axes,
    clip = clip, default = default, z_scales = match.arg(z_scales, c("fixed", "free")),
    config = list(axes = unique(axes), panes = unique(panes), box = match.arg(box, c("back", "all", "none")),
                  light = light, backend = backend, crs = crs,
                  frame = match.arg(frame, c("view", "azimuth", "sphere")), frame_registry = new.env(parent = emptyenv())))
}

Coord3D <- ggplot2::ggproto("Coord3D", ggplot2::Coord,
  is_free = function() TRUE,
  is_linear = function() FALSE,
  aspect = function(ranges) 1,
  setup_params = function(self, data) list(),
  setup_layout = function(self, layout, params) {
    layout$SCALE_Z <- if (self$z_scales == "free") seq_len(nrow(layout)) else rep.int(1L, nrow(layout))
    triples <- paste(layout$SCALE_X, layout$SCALE_Y, layout$SCALE_Z, sep = ":")
    layout$COORD <- match(triples, unique(triples))
    layout
  },
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    c(axis_view_3d(scale_x, self$limits$x, self$expand_axes[["x"]], "x"),
      axis_view_3d(scale_y, self$limits$y, self$expand_axes[["y"]], "y"))
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
  if (is.null(data$z)) data$z <- rep(0, nrow(data))
  normal <- normalise_positions_3d(data, panel_params)
  coordinates <- normal[c("x", "y", "z")]
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
    viewport <- panel_params$viewport %or3d% new_scene_viewport(100, 100)
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
