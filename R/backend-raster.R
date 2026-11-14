utils::globalVariables(c("raster_prepare_cpp", "raster_draw_cpp", "raster_canvas_cpp"))

# `edl`: eye-dome lighting strength of every primitive that is not a splat;
# splats keep the strength of their layer.
raster_colours_3d <- function(packet, prepared, light, edl = 0) {
  lapply(c("faces", "polylines", "markers", "text"), function(kind) {
    tab <- packet[[kind]]
    styles <- packet$styles[match(tab$style_id, packet$styles$style_id), , drop = FALSE]
    colour <- style_value_3d(styles, if (kind == "faces") "fill" else "colour", NA_character_)
    if (kind == "markers") {
      filled <- style_value_3d(styles, "shape", 19) %in% 21:25
      colour[filled] <- style_value_3d(styles, "fill", NA_character_)[filled]
    }
    smooth <- NULL
    if (kind == "faces") {
      shade <- which(style_value_3d(styles, "shade", FALSE))
      normals <- prepared$normals
      fragments <- list(normal_x = normals[, 1L], normal_y = normals[, 2L], normal_z = normals[, 3L])
      if (length(colour) && any(corner_attribute_columns_3d %in% names(packet$corners))) {
        lit <- !is.null(light) & style_value_3d(styles, "shade", FALSE) %in% TRUE
        smooth <- list(base = linear_rgb_3d(colour),
          lit = lit, normal = face_shading_normals_3d(fragments, seq_along(colour), prepared$camera, styles),
          light = if (!is.null(light)) resolved_light_3d(light, prepared$camera),
          toward = view_toward_3d(prepared$camera))
        smooth$normal[!is.finite(smooth$normal)] <- 0
      }
      colour[shade] <- shade_fill_3d(colour[shade], fragments, shade, prepared$camera, light, styles[shade, , drop = FALSE])
    }
    if (!length(colour)) return(matrix(numeric(), 4L, 0L))
    rgba <- colour_channels_3d(colour, alpha = TRUE) / 255
    rgba[4L, ] <- as.double(!is.na(colour))
    border <- style_value_3d(styles, "colour", NA_character_)
    outline <- colour_channels_3d(border, alpha = TRUE) / 255
    outline[4L, ] <- as.double(!is.na(border))
    attr(rgba, "outline") <- outline
    if (kind == "faces") attr(rgba, "sheet") <- as.integer(surface_sheets_3d(styles, tab$layer_id, tab$opacity))
    if (!is.null(smooth)) attr(rgba, "smooth") <- smooth
    metrics <- paint_metrics_3d(styles)
    attr(rgba, "width_mm") <- if (kind == "markers") metrics$border_mm * 2 else metrics$width_mm
    attr(rgba, "separate") <- if (kind == "markers") style_value_3d(styles, "shape", 19) %in% 21:25 else rep(FALSE, nrow(styles))
    strength <- rep(edl, nrow(styles))
    if (kind == "markers") {
      splat <- style_value_3d(styles, "render", "marker") %in% "splat"
      attr(rgba, "splat") <- splat
      layer <- as.double(style_value_3d(styles, "edl", 0))
      strength[splat] <- ifelse(is.na(layer[splat]), 0, layer[splat])
    }
    if (any(strength > 0)) attr(rgba, "edl") <- strength
    if (kind == "polylines") {
      arrows <- style_value_3d(styles, "arrow", list(NULL))
      attr(rgba, "arrows") <- t(vapply(arrows, function(a) {
        if (is.null(a)) return(c(0, 0, 0))
        c(grid::convertWidth(a$length, "mm", TRUE), a$angle * pi / 180, a$type)
      }, numeric(3)))
      attr(rgba, "arrow_fill") <- grDevices::col2rgb(style_value_3d(styles, "arrow_fill", border), alpha = TRUE) / 255
    }
    rgba
  })
}

# Worker threads of the tiled rasteriser: option `ggplot3d.threads`, 0 for one
# per hardware thread, at most two under R CMD check's core limit.
raster_threads_3d <- function() {
  threads <- getOption("ggplot3d.threads", 0L)
  scene_assert(is.numeric(threads) && length(threads) == 1L && is.finite(threads) &&
                 threads >= 0 && threads == trunc(threads),
               "option ggplot3d.threads must be a nonnegative integer")
  limit <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  if (nzchar(limit) && limit != "false") threads <- if (threads == 0) 2L else min(threads, 2L)
  as.integer(threads)
}

# Traced layers of a panel: the ray-march specs that layers attach to their
# (empty) packets, in layer order.
raster_traced_specs_3d <- function(packets) {
  Filter(Negate(is.null), lapply(packets, attr, "raymarch"))
}

# Per-sample traced input of a raster draw on `canvas`. Opaque traced
# surfaces (SDF layers, MIP and isosurface volumes) are found here, the
# nearest per sample, and shaded in linear light; absorption volumes travel
# with the sample rays and are integrated inside the compositing walk. Traced
# layer j has primitive id `first_id + j`, after the packet's primitives.
raster_traced_input_3d <- function(specs, camera, viewport, canvas, resolved, light, first_id) {
  grid <- list(width = canvas$width * canvas$antialias, height = canvas$height * canvas$antialias,
               extent = canvas$extent)
  rays <- raymarch_rays_3d(camera, viewport, grid, specs[[1L]]$ranges)
  n <- nrow(rays$origin)
  far <- (rays$far - rays$near) * rays$length
  depth <- rep(NA_real_, n); layer <- integer(n)
  colour <- matrix(0, n, 3L); normal <- matrix(NA_real_, n, 3L)
  absorption <- vapply(specs, function(s) identical(s$kind, "volume") && s$mode == 0L, logical(1))
  for (j in which(!absorption)) {
    spec <- specs[[j]]
    hit <- raymarch_surface_handlers_3d[[spec$kind %or3d% "sdf"]](spec, rays, far)
    z <- rays$near + hit$distance / rays$length
    take <- which(is.finite(z) & z <= rays$far & (is.na(depth) | z < depth))
    if (!length(take)) next
    span <- vapply(spec$ranges, diff, numeric(1))
    unit <- sweep(hit$normal[take, , drop = FALSE], 2L, span / resolved$model_scale, "*")
    unit <- unit / pmax(sqrt(rowSums(unit^2)), .Machine$double.eps)
    base <- if (is.null(hit$colour)) {
      matrix(srgb_decode_3d(colour_channels_3d(spec$fill) / 255), length(take), 3L, byrow = TRUE)
    } else hit$colour[take, , drop = FALSE]
    if (!identical(spec$mode, 1L) && !is.null(light))
      base <- shade_linear_3d(base, unit, resolved, light)
    depth[take] <- z[take]; layer[take] <- j
    colour[take, ] <- pmin(pmax(base, 0), 1); normal[take, ] <- unit
  }
  volumes <- specs[absorption]
  input <- list(depth = depth, colour = colour, normal = normal, layer = layer,
    ids = as.integer(first_id + seq_along(specs)), near = rays$near, far = rays$far,
    volumes = volumes, volume_layer = which(absorption))
  if (length(volumes)) input[c("origin", "direction", "length")] <- rays[c("origin", "direction", "length")]
  input
}

raster_scene_3d <- function(packet, camera, viewport, antialias = 2L, light = NULL,
                           id_pass = FALSE, buffers = FALSE, blend = "linear", traced = list(), edl = 0) {
  blend <- match.arg(blend, c("linear", "srgb"))
  validate_camera_3d(camera)
  prepared <- raster_prepare_cpp(packet[names(packet) != "styles"], camera, viewport)
  colours <- raster_colours_3d(packet, prepared, light, edl)
  line <- packet$polylines
  styles <- packet$styles[match(line$style_id, packet$styles$style_id), , drop = FALSE]
  lty <- style_value_3d(styles, "linetype", 1)
  dashes <- lapply(seq_along(lty), function(i) dash_pattern_3d(lty[i], line$width_mm[i]))
  attr(dashes, "hidden") <- lapply(line$width_mm, function(width) dash_pattern_3d("dashed", width))
  pixels <- ceiling(c(viewport$width_mm, viewport$height_mm) * viewport$dpi / 25.4)
  scene_assert(all(is.finite(pixels)) && all(pixels <= .Machine$integer.max), "Unrepresentable raster dimensions.")
  input <- list()
  if (length(traced)) {
    canvas <- raster_canvas_cpp(prepared$pointer, colours, dashes,
      as.integer(pixels[1L]), as.integer(pixels[2L]), as.integer(antialias))
    ids <- unlist(lapply(packet[c("faces", "polylines", "markers", "text")], `[[`, "primitive_id"), use.names = FALSE)
    input <- raster_traced_input_3d(traced, camera, viewport, canvas, prepared$camera, light, max(c(0L, ids)))
  }
  result <- raster_draw_cpp(prepared$pointer, colours, dashes,
    as.integer(pixels[1L]), as.integer(pixels[2L]), as.integer(antialias), id_pass,
    raster_threads_3d(), buffers, blend == "linear", input)
  if (length(traced)) result$traced <- data.frame(primitive_id = input$ids,
    layer_id = vapply(traced, function(s) as.integer(s$layer_id %or3d% NA_integer_), integer(1)),
    kind = vapply(traced, function(s) s$kind %or3d% "sdf", character(1)))
  result$engine_seconds <- prepared$engine_seconds
  result$camera <- prepared$camera
  result$viewport <- viewport
  result
}

raster_text_3d <- function(result, packet) {
  text <- result$text
  lapply(seq_along(text$row), function(i) {
    row <- text$row[i]; tab <- packet$text
    styles <- packet$styles[match(tab$style_id[row], packet$styles$style_id), , drop = FALSE]
    text_grob_3d(styles, text$x[i], text$y[i], tab$hjust[row], tab$vjust[row], tab$angle[row], tab$opacity[row])
  })
}

raster_image_grob_3d <- function(result) {
  extent <- result$extent
  grid::rasterGrob(result$image, x = mean(extent[c(1L, 3L)]), y = mean(extent[c(2L, 4L)]),
    width = grid::unit(extent[3L] - extent[1L], "npc"),
    height = grid::unit(extent[4L] - extent[2L], "npc"), interpolate = FALSE, name = "scene-raster-image-3d")
}

# The image of a raster result and the text labels it leaves visible.
raster_layer_children_3d <- function(result, packet) {
  c(list(raster_image_grob_3d(result)), raster_text_3d(result, packet))
}

# Panel assembly of every image-producing path (raster, ray-marched, path
# traced): the furniture underlay, the image with its text, the furniture
# overlay. The two furniture parts carry distinct names: a gTree draws its
# children by name.
raster_panel_grob_3d <- function(result, packet, furniture, name) {
  scene <- scene_compile(furniture$packet, furniture$camera, furniture$viewport)
  part <- function(partition, name) {
    selected <- scene
    selected$draw_order <- scene$draw_order[scene$fragments$partition[scene$draw_order] %in% partition]
    g <- render_scene_grid(selected, furniture$viewport)
    g$name <- name
    g
  }
  children <- c(list(part(c(0L, 1L), "scene-raster-underlay-3d")), raster_layer_children_3d(result, packet),
    list(part(2L, "scene-raster-overlay-3d")))
  list(scene = scene, grob = grid::gTree(children = do.call(grid::gList, children),
    raster_result = result, name = name))
}

render_raster_panel_3d <- function(x, furniture, backend) {
  traced <- raster_traced_specs_3d(x$packets)
  packet <- merge_scene_packets(x$packets, x$panel_params$panel_id)
  result <- raster_scene_3d(packet, furniture$camera, furniture$viewport,
    backend$antialias, x$panel_params$coord_params$light, buffers = length(traced) > 0L,
    blend = backend$blend, traced = traced, edl = depth_cue_edl_3d(x$panel_params))
  raster_panel_grob_3d(result, packet, furniture, if (length(traced)) "scene-raymarch-3d" else "scene-raster-3d")
}
