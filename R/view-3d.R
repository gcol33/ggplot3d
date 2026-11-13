#' Explore a scene with orbit controls
#'
#' Draw a panel in a WebGL htmlwidget, using its mapped scene and initial camera.
#' Drag to orbit, use the wheel or pinch to zoom, and select Camera code to copy
#' the current angles into a coord_3d() call. Arrow keys orbit; + and - zoom.
#' @param plot A ggplot with [coord_3d()].
#' @param width,height Widget dimensions in pixels, default 640 each.
#' @param panel Panel index in layout order, default 1. Facet strips, legends,
#'   plot titles and ordinary two-dimensional annotation grobs stay in the
#'   static plot. Each widget displays one panel.
#' @param elementId Optional HTML element identifier, default NULL. In Shiny,
#'   camera changes set the input named paste0(elementId, "_camera").
#' @param link Optional name of a link group, default NULL. Viewers on one
#'   page with the same `link` move together: an orbit, zoom or reset in one
#'   applies to all, azimuth, elevation and roll changes add and zoom factors
#'   multiply, so each viewer keeps its own view. Linking the panels of a
#'   [facet_views()] plot turns the front, top and side views as one object.
#' @param path A camera path from [camera_path_3d()], [orbit_3d()],
#'   [fly_through_3d()] or [dolly_zoom_3d()], or NULL (default). The viewer
#'   gains a Play button that moves the camera along the path with the views
#'   [animate_3d()] renders; dragging or Reset stops it.
#' @param fps Camera views per second of the sampled path, default 30.
#' @return An htmlwidget of class ggplot3d_view. Its x contains schema_version
#'   (2), scene (schema-1 indexed model geometry and mapped materials),
#'   furniture (with the panes each primitive belongs to and their outward
#'   normals), axes (candidate placements per guide), bounds, camera, framing,
#'   viewport, reference, background, capabilities and link, and with `path`
#'   the sampled camera views. Geometry is transferred once; camera
#'   interaction and playback stay in the browser.
#' @details Requires htmlwidgets. Opaque geometry uses a WebGL depth buffer.
#'   Transparent primitives are sorted back to front by centre depth; crossing
#'   transparent surfaces can differ from the exact static vector backend.
#'   Text uses plain browser text, with whole-anchor visibility for scene labels.
#'   Lighting uses the same linear-light colour calculation as static output.
#'   Furniture and its labels rotate with the camera. No external scripts load.
#' @examples
#' if (requireNamespace("htmlwidgets", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
#'     geom_point_3d(size = 3) + coord_3d()
#'   view_3d(p)
#'   views <- p + facet_views(c("front", "isometric"))
#'   front <- view_3d(views, panel = 1, width = 320, height = 320, link = "cars")
#'   iso <- view_3d(views, panel = 2, width = 320, height = 320, link = "cars")
#'   view_3d(p, path = orbit_3d(degrees = 360, duration = 6))
#' }
#' @export
view_3d <- function(plot, width = 640L, height = 640L, panel = 1L,
                    elementId = NULL, link = NULL, path = NULL, fps = 30) {
  motion_require_3d("htmlwidgets")
  motion_plot_coord_3d(plot)
  plot <- mono_plot_3d(plot)
  motion_dimensions_3d(width, height, 96)
  scene_assert(is.numeric(panel) && length(panel) == 1L && is.finite(panel) &&
    panel >= 1 && panel == trunc(panel), "panel must be a positive integer")
  scene_assert(is.null(link) || (is.character(link) && length(link) == 1L && !is.na(link) && nzchar(link)),
    "link must be NULL or one group name")
  scene_assert(is.null(path) || inherits(path, "ggplot3d_camera_path"),
    "path must be a camera path such as camera_path_3d() or orbit_3d()")
  scene_assert(is.numeric(fps) && length(fps) == 1L && is.finite(fps) && fps > 0,
    "fps must be a positive finite number")
  scratch <- tempfile(fileext = ".png")
  on.exit(unlink(scratch), add = TRUE)
  device <- motion_open_device_3d("png", scratch, width, height, 96)
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE, after = FALSE)
  built <- ggplot2::ggplotGrob(plot)
  panels <- motion_panels_3d(built)
  scene_assert(panel <= length(panels), "panel is outside the plot layout")
  # Furniture layout supplies authoritative physical margins and text metrics.
  forced <- grid::grid.force(built)
  selected <- motion_panels_3d(forced)[[panel]]
  params <- selected$panel_params
  outer <- selected$outer_viewport
  furniture <- panel_furniture_3d(params, selected$theme, outer, as.list(selected$frame_group))
  frame <- frame_of_3d(params)
  all_params <- params
  all_params$coord_params$panes <- vapply(frame$panes(), `[[`, character(1), "name")
  all_params$coord_params$box <- "all"
  all_furniture <- panel_furniture_3d(all_params, selected$theme, outer, as.list(selected$frame_group))
  packet <- merge_scene_packets(resolve_glyph_packets_3d(selected$packets, furniture$camera, furniture$viewport), params$panel_id)
  furniture_view <- view_packet_3d(all_furniture$packet)
  furniture_view$materials <- view_furniture_tags_3d(frame, all_furniture, furniture_view$materials)
  axes <- lapply(names(params$axis_guides), function(name) {
    guide <- params$axis_guides[[name]]
    m <- furniture$overlay$fit$metrics[[name]]
    list(axis = match(guide$axis, c("x", "y", "z")) - 1L,
      visible = guide_on_box_3d(guide, params),
      candidates = view_axis_candidates_3d(frame, guide),
      position = guide$params$position, secondary = guide$secondary,
      values = unname(guide$position), minor = unname(guide$minor),
      labels = view_labels_3d(guide$labels), title = view_labels_3d(guide$title),
      text = view_text_style_3d(m$text), title_style = view_text_style_3d(m$title),
      line = view_line_style_3d(element_3d(paste0("axis.line.", guide$axis), guide$theme)),
      ticks = view_line_style_3d(element_3d(paste0("axis.ticks.", guide$axis), guide$theme)),
      angle = m$angle, tick_length = m$tick_length, gap = m$gap,
      title_gap = m$title_gap, label_extent = m$label_extent,
      label_size = unname(rotated_extent_3d(m$label, m$angle)), title_size = unname(m$title_size),
      dodge = guide$params$n.dodge, minor_ticks = isTRUE(guide$params$minor.ticks),
      thin = thin_labels_of_3d(guide), check_overlap = isTRUE(guide$params$check.overlap),
      cap = guide$params$cap)
  })
  bg <- element_3d("panel.background", selected$theme)
  raw_camera <- unclass(params$camera)
  # Automatic camera values use JSON null; finite resolved values travel separately.
  raw_camera[c("distance", "near", "far")] <- lapply(raw_camera[c("distance", "near", "far")],
    function(value) if (is.na(value) || !is.finite(value)) NULL else value)
  x <- list(schema_version = 2L, scene = view_packet_3d(packet),
    furniture = furniture_view, axes = unname(axes),
    bounds = unname(lapply(seq_len(nrow(frame$bounds())), function(i) unname(frame$bounds()[i, ]))),
    camera = raw_camera, link = link,
    framing = list(frame = params$coord_params$frame,
      zoom_scale = furniture$camera$zoom / params$camera$zoom,
      origin = unname(furniture$overlay$fit$origin),
      panes = params$coord_params$panes, box = params$coord_params$box,
      back = as.list(frame$back_candidates()), floor = as.list(frame$floor_panes())),
    viewport = c(furniture$viewport, list(outer_width_mm = outer$width_mm,
      outer_height_mm = outer$height_mm)),
    reference = list(corners_mm = unname(selected$furniture$corners_mm)),
    light = params$coord_params$light,
    background = if (visible_element_3d(bg)) view_rgba_3d(bg$fill) else c(1, 1, 1, 1),
    capabilities = list(opaque_visibility = "depth-buffer",
      transparency = "primitive-centroid", text = "plain-anchor", geometry_schema = 1L))
  if (!is.null(path)) x$path <- view_path_3d(path, fps, params, furniture$viewport, frame$bounds())
  grDevices::dev.off(device)
  htmlwidgets::createWidget("ggplot3d_view", x, width = width, height = height,
    package = "ggplot3d", elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(padding = 0, browser.fill = FALSE))
}

# Camera views of a path for the viewer to play, as columns, with the frame
# step in seconds and the magnification of the path's framing.
view_path_3d <- function(path, fps, params, viewport, bounds) {
  camera <- params$camera
  context <- motion_path_context_3d(camera, params,
    function() min(1, viewport$width_mm / viewport$height_mm))
  sampled <- path_sample_3d(path, NULL, fps, context)
  cameras <- lapply(sampled$steps, motion_camera_3d, base = camera)
  fitted <- frame_camera_3d(cameras[[1L]], viewport, motion_frame_mode_3d(sampled$steps), bounds)
  table <- motion_camera_table_3d(cameras, sampled$times)
  table$distance[is.na(table$distance)] <- -1
  shear <- do.call(rbind, lapply(cameras, `[[`, "shear"))
  c(as.list(table), list(shear_x = shear[, 1L], shear_y = shear[, 2L],
    step = if (length(sampled$times) > 1L) sampled$times[2L] else 1,
    loop = sampled$closed, zoom_scale = fitted$zoom / cameras[[1L]]$zoom))
}

# Pane membership and outward model normals of furniture primitives, so the
# viewer can choose rear panes and rear edges for each camera it shows.
view_furniture_tags_3d <- function(frame, furniture, materials) {
  panes <- frame$panes()
  names(panes) <- vapply(panes, `[[`, character(1), "name")
  for (i in seq_along(furniture$tags)) {
    tag <- furniture$tags[[i]]
    if (is.null(tag)) next
    at <- matrix(tag$at, 1L)
    normals <- lapply(tag$panes, function(name) unname(pane_normals_3d(frame, panes[[name]], at, furniture$camera)[1L, ]))
    materials[[i]]$frame <- list(type = tag$type, panes = as.list(tag$panes), normals = normals)
  }
  materials
}

# Candidate placements of an axis guide in model space: the path, tick and
# minor tick anchors, neighbours that give the local direction of a curved
# path, and the vertex that carries the title of a simplex part.
view_axis_candidates_3d <- function(frame, guide) {
  candidates <- frame$axis_candidates(guide)
  rows <- function(m) unname(lapply(seq_len(nrow(m)), function(i) unname(m[i, ])))
  at <- function(candidate, values) frame$map(axis_candidate_points_3d(candidate, values))
  out <- lapply(candidates, function(candidate) {
    xyz <- rbind(candidate$start, candidate$end)
    path <- if (frame$linear) frame$map(xyz) else polyline_model_3d(frame, xyz)$model
    values <- guide$position
    entry <- list(path = rows(path), ticks = rows(at(candidate, values)), minor = rows(at(candidate, guide$minor)))
    if (nrow(path) > 2L && length(values)) {
      entry$before <- rows(at(candidate, values - 1e-4)); entry$after <- rows(at(candidate, values + 1e-4))
    }
    title <- candidate$title
    if (identical(title$type, "vertex")) entry$title_at <- unname(drop(frame$map(matrix(title$at, 1L))))
    entry
  })
  list(auto = attr(candidates, "auto") %or3d% "lower", options = unname(out))
}

view_rgba_3d <- function(colour) {
  if (is.null(colour) || is.na(colour)) return(c(0, 0, 0, 0))
  unname(grDevices::col2rgb(colour, alpha = TRUE)[, 1L] / 255)
}

view_labels_3d <- function(labels) {
  vapply(seq_along(labels), function(i) {
    if (is.expression(labels) || is.language(labels[[i]])) paste(deparse(labels[[i]]), collapse = " ")
    else as.character(labels[[i]])
  }, character(1))
}

view_text_style_3d <- function(element) {
  visible <- visible_element_3d(element)
  if (!visible) element <- list()
  list(visible = visible,
    colour = view_rgba_3d(element$colour), size = element$size %or3d% 11,
    family = element$family %or3d% "sans-serif", face = element$face %or3d% "plain")
}

view_line_style_3d <- function(element) {
  visible <- visible_element_3d(element)
  if (!visible) element <- list()
  list(visible = visible, colour = view_rgba_3d(element$colour),
    width = element$linewidth %or3d% 0.5)
}

view_packet_3d <- function(packet) {
  validate_scene_packet(packet)
  styles <- packet$styles
  metrics <- paint_metrics_3d(styles)
  materials <- lapply(seq_len(nrow(styles)), function(i) {
    s <- styles[i, , drop = FALSE]
    get <- function(key, default) style_value_3d(s, key, default)[[1L]]
    normal <- if (all(c("normal_x", "normal_y", "normal_z") %in% names(s)))
      unname(as.numeric(unlist(s[1L, c("normal_x", "normal_y", "normal_z")], use.names = FALSE))) else NULL
    if (!length(normal) || any(!is.finite(normal)) || sum(normal^2) == 0) normal <- NULL
    dash <- dash_pattern_3d(get("linetype", 1), metrics$width_mm[i])
    dash <- dash[is.finite(dash)]
    list(fill = view_rgba_3d(get("fill", NA_character_)),
      colour = view_rgba_3d(get("colour", NA_character_)),
      shape = get("shape", 19), width_mm = metrics$width_mm[i],
      border_mm = metrics$border_mm[i],
      linetype = get("linetype", 1), shade = get("shade", FALSE),
      normal = normal,
      label = view_labels_3d(list(get("label", "")))[[1L]],
      fontsize = get("fontsize", get("size", 3.88) * 72.27 / 25.4),
      family = get("family", "sans-serif"), fontface = get("fontface", 1),
      anchor = get(".anchor", FALSE),
      dash = dash)
  })
  packet$styles <- NULL
  packet$materials <- unname(materials)
  unclass(packet)
}
