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
#' @return An htmlwidget of class ggplot3d_view. Its x contains schema_version,
#'   scene (schema-1 indexed model geometry and mapped materials), furniture,
#'   axes, camera, framing, viewport, reference, background and capabilities. Geometry is
#'   transferred once; camera interaction stays in the browser.
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
#' }
#' @export
view_3d <- function(plot, width = 640L, height = 640L, panel = 1L,
                    elementId = NULL) {
  motion_require_3d("htmlwidgets")
  motion_plot_coord_3d(plot)
  motion_dimensions_3d(width, height, 96)
  scene_assert(is.numeric(panel) && length(panel) == 1L && is.finite(panel) &&
    panel >= 1 && panel == trunc(panel), "panel must be a positive integer")
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  motion_png_3d(path, width, height, 96)
  device <- grDevices::dev.cur()
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
  all_params <- params
  all_params$coord_params$panes <- unique(c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax"))
  all_params$coord_params$box <- "all"
  all_furniture <- panel_furniture_3d(all_params, selected$theme, outer, as.list(selected$frame_group))
  packet <- merge_scene_packets(selected$packets, params$panel_id)
  axes <- lapply(names(params$axis_guides), function(name) {
    guide <- params$axis_guides[[name]]
    m <- furniture$overlay$fit$metrics[[name]]
    list(axis = match(guide$axis, c("x", "y", "z")) - 1L,
      visible = guide$axis %in% params$coord_params$axes,
      position = guide$params$position, secondary = guide$secondary,
      values = unname(guide$position), minor = unname(guide$minor),
      labels = view_labels_3d(guide$labels), title = view_labels_3d(guide$title),
      text = view_text_style_3d(m$text), title_style = view_text_style_3d(m$title),
      line = view_line_style_3d(element_3d(paste0("axis.line.", guide$axis), guide$theme)),
      ticks = view_line_style_3d(element_3d(paste0("axis.ticks.", guide$axis), guide$theme)),
      angle = m$angle, tick_length = m$tick_length, gap = m$gap,
      title_gap = m$title_gap, label_extent = m$label_extent,
      label_size = unname(m$label), title_size = unname(m$title_size),
      dodge = guide$params$n.dodge, minor_ticks = isTRUE(guide$params$minor.ticks),
      cap = guide$params$cap)
  })
  bg <- element_3d("panel.background", selected$theme)
  raw_camera <- unclass(params$camera)
  # Automatic camera values use JSON null; finite resolved values travel separately.
  raw_camera[c("distance", "near", "far")] <- lapply(raw_camera[c("distance", "near", "far")],
    function(value) if (is.na(value) || !is.finite(value)) NULL else value)
  x <- list(schema_version = 1L, scene = view_packet_3d(packet),
    furniture = view_packet_3d(all_furniture$packet), axes = unname(axes),
    camera = raw_camera,
    framing = list(frame = params$coord_params$frame,
      zoom_scale = furniture$camera$zoom / params$camera$zoom,
      origin = unname(furniture$overlay$fit$origin),
      panes = params$coord_params$panes, box = params$coord_params$box),
    viewport = c(furniture$viewport, list(outer_width_mm = outer$width_mm,
      outer_height_mm = outer$height_mm)),
    reference = list(corners_mm = unname(selected$furniture$corners_mm)),
    light = params$coord_params$light,
    background = if (visible_element_3d(bg)) view_rgba_3d(bg$fill) else c(1, 1, 1, 1),
    capabilities = list(opaque_visibility = "depth-buffer",
      transparency = "primitive-centroid", text = "plain-anchor", geometry_schema = 1L))
  grDevices::dev.off(device)
  htmlwidgets::createWidget("ggplot3d_view", x, width = width, height = height,
    package = "ggplot3d", elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(padding = 0, browser.fill = FALSE))
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
