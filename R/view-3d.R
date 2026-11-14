#' Explore a plot with orbit controls, tooltips and selection
#'
#' Draw a plot in a WebGL htmlwidget, using its mapped scenes and initial
#' cameras. Drag to orbit, use the wheel or pinch to zoom, and select Camera
#' code to copy the current angles into a coord_3d() call. Arrow keys orbit;
#' + and - zoom. Hovering a primitive shows its tooltip; clicking selects it.
#'
#' Interaction follows [ggiraph](https://davidgohel.github.io/ggiraph/) for
#' aesthetics, [deck.gl](https://deck.gl/docs/api-reference/core/layer) for
#' picking and [crosstalk](https://rstudio.github.io/crosstalk/) for linked
#' selection. Every `_3d` layer accepts two extra aesthetics: `tooltip`, the
#' text shown on hover, and `data_id`, the selection key; rows that share a
#' key are selected together, across layers. Without a `tooltip` mapping, the
#' tooltip lists the mapped aesthetics of the row under the pointer. Without a
#' `data_id`, each row of each layer is its own key, written `"<layer>:<row>"`.
#' Map these aesthetics in [ggplot2::ggplot()] when a layer comes from
#' ggplot2 itself, whose layer constructors do not know them.
#'
#' The browser holds one interleaved vertex buffer per primitive kind (faces,
#' stroke segments, marker footprints) and identifies the primitive under the
#' pointer from an offscreen id image, by the same `primitive_id` the raster
#' backend returns in its id buffer.
#' @param plot A ggplot with [coord_3d()].
#' @param data Optional crosstalk `SharedData` whose `origData()` the plot
#'   draws, default NULL. Layers that draw these rows take their selection
#'   keys from it, share selections with the other crosstalk widgets of its
#'   group, and hide rows that a crosstalk filter removes.
#' @param width,height Widget dimensions in pixels, default 640 each.
#' @param panel NULL (default) shows the whole plot: every panel is live, and
#'   facet strips, legends, titles and the other parts outside the panels are
#'   embedded as SVG around them (requires svglite). A panel index in layout
#'   order shows that panel alone.
#' @param link Linked cameras, default TRUE: orbiting or zooming one panel
#'   turns every panel by the same amount, keeping their offsets. FALSE moves
#'   the panel under the pointer alone. A group name links the panels and
#'   also links this viewer with the other viewers on the page that carry
#'   the same name: an orbit, zoom or reset in one applies to all, azimuth,
#'   elevation and roll changes add and zoom factors multiply, so each viewer
#'   keeps its own view. Linking the panels of a [facet_views()] plot shown
#'   in separate viewers turns the front, top and side views as one object.
#' @param selection `"multiple"` (default) lets shift- or control-click add
#'   and remove keys, `"single"` keeps one key, `"none"` disables selection.
#'   Unselected primitives are dimmed while a selection exists.
#' @param tooltip Default tooltips listing mapped values, default TRUE. A
#'   mapped `tooltip` aesthetic is always shown.
#' @param elementId Optional HTML element identifier, default NULL. In Shiny,
#'   camera changes set the input named `paste0(elementId, "_camera")` and
#'   selections set `paste0(elementId, "_selected")` to the selected keys.
#' @param path A camera path from [camera_path_3d()], [orbit_3d()],
#'   [fly_through_3d()] or [dolly_zoom_3d()], or NULL (default). The viewer
#'   gains a Play button that moves every panel's camera along the path with
#'   the views [animate_3d()] renders; dragging or Reset stops it.
#' @param fps Camera views per second of the sampled path, default 30.
#' @return An htmlwidget of class ggplot3d_view. Its x contains schema_version
#'   (4), frame (the plot size in mm), chrome (the SVG of everything outside
#'   the panels, or NULL), panels, link (whether the panels share camera
#'   moves), link_group (the group name, or NULL), selection settings and
#'   capabilities.
#'   Each panel holds its rectangle in the frame, scene and furniture
#'   (schema-1 geometry whose table columns are base64 little-endian arrays,
#'   with mapped materials, the alpha_mode sheet key of every face, and
#'   quadric rows with their placement after camera pre and its inverse),
#'   axes, bounds, camera, framing, viewport, light,
#'   background, layers (the source rows, keys and tooltips of its
#'   primitives) and, with `path`, the sampled camera views. Geometry is
#'   transferred once; camera interaction and playback stay in the browser.
#' @details Requires htmlwidgets and a browser with WebGL 2. Opaque geometry
#'   uses a depth buffer. Transparent geometry uses
#'   weighted blended order-independent transparency in linear light, an
#'   approximation requiring WebGL 2's `EXT_color_buffer_float` extension.
#'   Faces drawn with `alpha_mode = "surface"` enter it once per sheet: a
#'   depth pass per sheet keeps only the nearest fragment of each sheet, so a
#'   sheet folding over itself does not darken while other sheets show
#'   through it. Sheets whose screen extents do not overlap share a pass;
#'   past 32 passes, the remaining sheets blend face by face.
#'   Spheres, cylinders and cones are ray cast (Sigg et al. 2006): each
#'   pixel's ray meets the exact surface, which writes its own depth and is
#'   lit at its exact normal; a translucent one enters the transparency once
#'   per side.
#'   Strokes and markers widen to their physical size
#'   in the vertex shader, and face lighting uses the same linear-light colour
#'   calculation as static output. Text uses plain browser text drawn over
#'   the scene, hidden only when its anchor is clipped. Furniture and its labels rotate
#'   with the camera. The widget element receives `ggplot3d:camera`,
#'   `ggplot3d:hover` and `ggplot3d:select` events. No external scripts load.
#'
#'   A discrete `tooltip` or `data_id` mapping does not split groups, so paths
#'   and surfaces keep their shape.
#' @examples
#' if (requireNamespace("htmlwidgets", quietly = TRUE) &&
#'     requireNamespace("svglite", quietly = TRUE)) {
#'   cars <- data.frame(mtcars, model = rownames(mtcars))
#'   p <- ggplot2::ggplot(cars, ggplot2::aes(wt, mpg, z = hp,
#'       colour = factor(cyl), tooltip = model, data_id = model)) +
#'     geom_point_3d(size = 3) + coord_3d() + ggplot2::facet_wrap(~am)
#'   view_3d(p, width = 720, height = 400)
#' }
#' if (requireNamespace("htmlwidgets", quietly = TRUE) &&
#'     requireNamespace("crosstalk", quietly = TRUE) &&
#'     requireNamespace("svglite", quietly = TRUE)) {
#'   shared <- crosstalk::SharedData$new(mtcars)
#'   p <- ggplot2::ggplot(shared$origData(), ggplot2::aes(wt, mpg, z = hp)) +
#'     geom_point_3d() + coord_3d()
#'   view_3d(p, data = shared)
#' }
#' if (requireNamespace("htmlwidgets", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
#'     geom_point_3d(size = 3) + coord_3d()
#'   views <- p + facet_views(c("front", "isometric"))
#'   front <- view_3d(views, panel = 1, width = 320, height = 320, link = "cars")
#'   iso <- view_3d(views, panel = 2, width = 320, height = 320, link = "cars")
#'   view_3d(p, panel = 1, path = orbit_3d(degrees = 360, duration = 6))
#' }
#' @export
view_3d <- function(plot, data = NULL, width = 640L, height = 640L, panel = NULL,
                    link = TRUE, selection = c("multiple", "single", "none"),
                    tooltip = TRUE, elementId = NULL, path = NULL, fps = 30) {
  motion_require_3d("htmlwidgets")
  motion_plot_coord_3d(plot)
  plot <- mono_plot_3d(plot)
  motion_dimensions_3d(width, height, 96)
  selection <- match.arg(selection)
  scene_assert(is.null(panel) || (is.numeric(panel) && length(panel) == 1L && is.finite(panel) &&
    panel >= 1 && panel == trunc(panel)), "panel must be a positive integer")
  scene_assert(isTRUE(link) || isFALSE(link) ||
    (is.character(link) && length(link) == 1L && !is.na(link) && nzchar(link)),
    "link must be TRUE, FALSE or one group name")
  scene_assert(isTRUE(tooltip) || isFALSE(tooltip), "tooltip must be TRUE or FALSE")
  scene_assert(is.null(path) || inherits(path, "ggplot3d_camera_path"),
    "path must be a camera path such as camera_path_3d() or orbit_3d()")
  scene_assert(is.numeric(fps) && length(fps) == 1L && is.finite(fps) && fps > 0,
    "fps must be a positive finite number")
  shared <- view_shared_data_3d(data)
  if (is.null(panel)) motion_require_3d("svglite")
  scratch <- tempfile(fileext = ".png")
  on.exit(unlink(scratch), add = TRUE)
  device <- motion_open_device_3d("png", scratch, width, height, 96)
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE, after = FALSE)
  built <- ggplot2::ggplot_build(plot)
  table <- ggplot2::ggplot_gtable(built)
  count <- length(motion_panels_3d(table))
  scene_assert(count > 0L, "plot has no 3D scene panels")
  scene_assert(is.null(panel) || panel <= count, "panel is outside the plot layout")
  if (!is.null(panel)) table <- view_panel_layout_3d(table, panel)
  # Furniture layout supplies authoritative physical margins and text metrics.
  forced <- layout_scene_panels_3d(table)
  device_mm <- unname(grDevices::dev.size("in") * 25.4)
  layers <- view_layers_3d(built, shared, tooltip)
  panels <- lapply(seq_along(forced), function(i) {
    selected <- forced[[i]]
    view <- view_panel_3d(selected, path, fps)
    size <- c(selected$outer_viewport$width_mm, selected$outer_viewport$height_mm)
    view$rect_mm <- c(if (is.null(panel)) selected$device_origin_in * 25.4 else c(0, 0), size)
    view$layers <- view_panel_layers_3d(layers, selected$panel_params$panel_id)
    view
  })
  grDevices::dev.off(device)
  frame <- if (is.null(panel)) device_mm else panels[[1L]]$rect_mm[3:4]
  x <- list(schema_version = 4L, shade_glsl = shade_glsl_cpp(view_max_lights_3d),
    frame = list(width_mm = frame[1L], height_mm = frame[2L]),
    chrome = if (is.null(panel)) view_chrome_3d(table, width, height) else NULL,
    panels = panels, link = !isFALSE(link),
    link_group = if (is.character(link)) link else NULL,
    selection = list(mode = selection, dim = 0.2,
      group = if (is.null(shared)) NULL else shared$groupName()),
    capabilities = list(opaque_visibility = "depth-buffer",
      transparency = "weighted-blended", text = "plain-anchor", geometry_schema = 1L,
      picking = "id-buffer", buffers = "interleaved-per-kind", markers = "instanced-disc-quads",
      quadrics = "ray-cast"))
  htmlwidgets::createWidget("ggplot3d_view", x, width = width, height = height,
    package = "ggplot3d", elementId = elementId,
    dependencies = if (is.null(shared)) NULL else crosstalk::crosstalkLibs(),
    sizingPolicy = htmlwidgets::sizingPolicy(padding = 0, browser.fill = FALSE))
}

#' Shiny bindings for the interactive 3D viewer
#'
#' Place a viewer in a Shiny user interface and render a reactive expression
#' returning [view_3d()]. In a module, pass `ns("scene")` to `view3dOutput()`
#' and assign `output$scene` inside `shiny::moduleServer()`.
#' Camera updates arrive in `input$scene_camera`; selected data keys arrive
#' in `input$scene_selected`. Shiny supplies the widget identifier, so
#' `view_3d()` can leave `elementId` at its default.
#'
#' @param outputId Output identifier, namespaced in a module's user interface.
#' @param width,height CSS dimensions, default `"100%"` and `"400px"`.
#' @param expr An expression returning a `ggplot3d_view` htmlwidget.
#' @param env Environment in which to evaluate `expr`.
#' @param quoted Whether `expr` is already quoted.
#' @return `view3dOutput()` returns an HTML output container.
#'   `renderView3d()` returns a Shiny render function.
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE) &&
#'     requireNamespace("htmlwidgets", quietly = TRUE)) {
#'   ui <- shiny::fluidPage(view3dOutput("scene"))
#'   server <- function(input, output, session) {
#'     output$scene <- renderView3d({
#'       p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
#'         geom_point_3d() + coord_3d()
#'       view_3d(p, panel = 1)
#'     })
#'   }
#'   app <- shiny::shinyApp(ui, server)
#' }
#' @name view_3d-shiny
#' @export
view3dOutput <- function(outputId, width = "100%", height = "400px") {
  motion_require_3d("shiny")
  motion_require_3d("htmlwidgets")
  htmlwidgets::shinyWidgetOutput(outputId, "ggplot3d_view", width, height,
    package = "ggplot3d")
}

#' @rdname view_3d-shiny
#' @export
renderView3d <- function(expr, env = parent.frame(), quoted = FALSE) {
  motion_require_3d("shiny")
  motion_require_3d("htmlwidgets")
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, view3dOutput, env, quoted = TRUE)
}

# A standalone panel retains its layout aspect and measures furniture at the
# widget size, with its own guides and camera supplying the framing group.
view_panel_layout_3d <- function(table, panel) {
  cells <- which(vapply(table$grobs, function(g) length(motion_panels_3d(g)) > 0L, logical(1)))
  cell <- table$layout[cells[panel], ]
  selected <- table[cell$t:cell$b, cell$l:cell$r]
  selected$grobs <- lapply(selected$grobs, function(g) {
    if (!length(motion_panels_3d(g))) return(grid::nullGrob())
    motion_map_panels_3d(g, function(p) {
      p$frame_group <- list(list(params = p$panel_params, theme = p$theme))
      p
    })
  })
  selected
}

# One panel of the viewer: its scene, furniture for every camera, axis
# candidates and initial camera.
view_panel_3d <- function(selected, path = NULL, fps = 30) {
  params <- selected$panel_params
  outer <- selected$outer_viewport
  furniture <- selected$layout_furniture
  frame <- frame_of_3d(params)
  all_params <- params
  all_params$coord_params$panes <- vapply(frame$panes(), `[[`, character(1), "name")
  all_params$coord_params$box <- "all"
  all_furniture <- panel_furniture_3d(all_params, selected$theme, outer, as.list(selected$frame_group))
  packets <- resolve_draw_packets_3d(selected$packets, furniture$camera, furniture$viewport)
  packets <- resolve_glyph_packets_3d(packets, furniture$camera, furniture$viewport)
  packet <- merge_scene_packets(packets, params$panel_id)
  furniture_view <- view_packet_3d(all_furniture$packet, params$camera$pre)
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
  list(id = params$panel_id, scene = view_packet_3d(packet, params$camera$pre), glyphs = view_glyphs_3d(packets),
    furniture = furniture_view, axes = unname(axes),
    bounds = unname(lapply(seq_len(nrow(frame$bounds())), function(i) unname(frame$bounds()[i, ]))),
    camera = raw_camera,
    framing = list(frame = params$coord_params$frame,
      zoom_scale = furniture$camera$zoom / params$camera$zoom,
      origin = unname(furniture$overlay$fit$origin),
      panes = params$coord_params$panes, box = params$coord_params$box,
      back = as.list(frame$back_candidates()), floor = as.list(frame$floor_panes())),
    viewport = c(furniture$viewport, list(outer_width_mm = outer$width_mm,
      outer_height_mm = outer$height_mm)),
    reference = list(corners_mm = unname(furniture_overlay_3d(furniture, NULL, selected$theme)$corners_mm)),
    light = view_lights_3d(panel_lights_3d(params$coord_params$light, params)),
    depth_cue = view_depth_cue_3d(params$coord_params$depth_cue, selected$theme),
    background = if (visible_element_3d(bg)) view_rgba_3d(bg$fill) else c(0, 0, 0, 0),
    path = if (is.null(path)) NULL else view_path_3d(path, fps, params, furniture$viewport, frame$bounds()))
}

# Deferred outline vertices keep their millimetre coordinates and orientation
# outside the compiled scene schema. Vertex indices refer to the merged scene.
view_glyphs_3d <- function(packets) {
  shift <- 0L; out <- list()
  for (packet in packets) {
    glyph <- attr(packet, "glyph_mm")
    if (!is.null(glyph$orientation)) {
      spec <- glyph$orientation
      columns <- c(list(centre = glyph$centre, local = glyph$local), spec[c("u", "v", "up")])
      columns <- lapply(columns, function(x) if (is.null(x)) NULL else view_column_3d(as.double(t(x))))
      out[[length(out) + 1L]] <- c(list(rows = view_column_3d(as.integer(glyph$rows + shift)),
        facing = spec$facing, angle = view_column_3d(spec$angle)), columns)
    }
    shift <- shift + length(packet$vertices$x)
  }
  out
}

# Everything the static plot draws outside its scene panels, as one SVG of
# the widget's size: strips, legends, titles, captions and the plot
# background.
view_chrome_3d <- function(table, width, height) {
  chrome <- motion_map_panels_3d(table, function(g) grid::nullGrob())
  svg <- svglite::svgstring(width = width / 96, height = height / 96, standalone = FALSE)
  device <- grDevices::dev.cur()
  on.exit(if (device %in% grDevices::dev.list()) grDevices::dev.off(device), add = TRUE)
  grid::grid.newpage()
  grid::grid.draw(chrome)
  grDevices::dev.off(device)
  as.character(svg())
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

# The viewer has no homogeneous clipper: it receives finite vertices with the
# camera pre already applied. Quadrics with a bounded image are ray cast in
# the browser and travel as quadric rows; the others become inscribed meshes.
view_packet_3d <- function(packet, pre = diag(4)) {
  cast <- view_quadrics_3d(packet$quadrics, pre)
  quadrics <- NULL
  if (length(cast$rows)) {
    quadrics <- lapply(packet$quadrics[c("layer_id", "source_row", "style_id", "partition", "opacity", "kind",
      "radius_ratio", "cap_0", "cap_1")], `[`, cast$rows)
    packet <- subset_quadrics_3d(packet, setdiff(seq_along(packet$quadrics$kind), cast$rows))
  }
  packet <- scene_finite_packet_3d(packet, pre)
  if (!is.null(quadrics)) {
    last <- max(0L, unlist(lapply(packet[primitive_kinds_3d], `[[`, "primitive_id")))
    packet$quadrics <- c(list(primitive_id = last + seq_along(cast$rows)), quadrics,
      list(to_box = as.double(cast$to_box), to_local = as.double(cast$to_local)))
  }
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
      alpha_mode = get("alpha_mode", "face"),
      dash = dash)
  })
  materials <- view_materials_3d(materials, styles, packet$materials)
  face_styles <- styles[match(packet$faces$style_id, styles$style_id), , drop = FALSE]
  sheet <- as.integer(surface_sheets_3d(face_styles, packet$faces$layer_id, packet$faces$opacity))
  packet$styles <- NULL
  packet$materials <- unname(materials)
  for (table in names(scene_schemas())) packet[[table]] <- lapply(packet[[table]], view_column_3d)
  packet <- unclass(packet)
  packet$face_sheet <- view_column_3d(sheet)
  packet
}

# Quadric rows the viewer ray casts (Sigg, Weyrich, Botsch and Gross 2006):
# those whose local bounding box lies at positive w after camera pre, so that
# the image is bounded and a screen rectangle covers it. Per row the
# placement composed with pre and its inverse, 16 column-major entries each.
view_quadrics_3d <- function(q, pre) {
  pre <- matrix(as.double(pre), 4L, 4L)
  n <- length(q$kind)
  to_box <- to_local <- matrix(0, 16L, n)
  keep <- logical(n)
  for (i in seq_len(n)) {
    m <- pre %*% matrix(vapply(quadric_transform_columns, function(column) q[[column]][i], numeric(1)), 4L, 4L)
    r <- max(1, if (q$kind[i] == 3L) q$radius_ratio[i] else 1)
    corners <- cbind(as.matrix(expand.grid(c(-r, r), c(-r, r), c(if (q$kind[i] == 1L) -1 else 0, 1))), 1)
    keep[i] <- all((m %*% t(corners))[4L, ] > 0)
    if (keep[i]) { to_box[, i] <- m; to_local[, i] <- solve(m) }
  }
  list(rows = which(keep), to_box = to_box[, keep, drop = FALSE], to_local = to_local[, keep, drop = FALSE])
}

# Lights the viewer can show at once (the uniform arrays of its kernel).
view_max_lights_3d <- 8L

# Kernel material of each viewer style: (ambient, diffuse, specular,
# shininess) and (specular colour reflectance, linear back colour or -1).
view_materials_3d <- function(materials, styles, table) {
  kernel <- kernel_materials_3d(table)
  index <- material_index_3d(styles)
  for (i in seq_along(materials)) {
    k <- index[i]
    if (k == 0L) next
    back <- kernel$back[k, ]
    materials[[i]]$material <- c(kernel$ambient[k], kernel$diffuse[k], kernel$specular[k], kernel$shininess[k])
    materials[[i]]$material2 <- c(kernel$specular_colour[k], if (all(is.finite(back))) back else c(-1, 0, 0))
  }
  materials
}

# Lights for the viewer, which resolves camera-space vectors per frame:
# kind, space, vector, linear colours times intensity and falloff, and the
# colour transfer as uTransfer = (transfer id, range, neutral cosine).
view_lights_3d <- function(light) {
  if (is.null(light)) return(NULL)
  if (length(light$lights) > view_max_lights_3d) warning("view_3d() shows the first ", view_max_lights_3d, " lights.", call. = FALSE)
  linear <- function(colour, intensity) unname(srgb_decode_3d(grDevices::col2rgb(colour)[, 1L] / 255) * intensity)
  transfer <- transfer_settings_3d(light)
  list(lights = lapply(light$lights, function(l) list(kind = l$kind, space = l$space, vector = l$vector,
      colour = linear(l$colour, l$intensity), ground = linear(l$ground, l$intensity), falloff = l$falloff)),
    ambient = rep(light$ambient, 3L), mode = light$mode, direction = light$direction, space = light$space,
    diffuse = light$diffuse, transfer = c(transfer$transfer, transfer$range, transfer$neutral),
    shadow = transfer$shadow, highlight = transfer$highlight)
}

# A numeric column as a base64 typed array: little-endian float32 for
# doubles, int32 for integers and uint8 for logicals.
view_column_3d <- function(x) {
  if (is.logical(x)) return(list(type = "u8", b64 = base64_3d(as.raw(as.integer(x)))))
  if (is.integer(x)) return(list(type = "i32", b64 = base64_3d(writeBin(x, raw(), size = 4L, endian = "little"))))
  list(type = "f32", b64 = base64_3d(writeBin(as.double(x), raw(), size = 4L, endian = "little")))
}

