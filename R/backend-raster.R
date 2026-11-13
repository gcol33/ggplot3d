utils::globalVariables(c("raster_prepare_cpp", "raster_draw_cpp"))

raster_colours_3d <- function(packet, prepared, light) {
  lapply(c("faces", "polylines", "markers", "text"), function(kind) {
    tab <- packet[[kind]]
    styles <- packet$styles[match(tab$style_id, packet$styles$style_id), , drop = FALSE]
    colour <- style_value_3d(styles, if (kind == "faces") "fill" else "colour", NA_character_)
    if (kind == "markers") {
      filled <- style_value_3d(styles, "shape", 19) %in% 21:25
      colour[filled] <- style_value_3d(styles, "fill", NA_character_)[filled]
    }
    if (kind == "faces") {
      shade <- which(style_value_3d(styles, "shade", FALSE))
      normals <- prepared$normals
      fragments <- list(normal_x = normals[, 1L], normal_y = normals[, 2L], normal_z = normals[, 3L])
      colour[shade] <- shade_fill_3d(colour[shade], fragments, shade, prepared$camera, light)
    }
    if (!length(colour)) return(matrix(numeric(), 4L, 0L))
    rgba <- grDevices::col2rgb(colour, alpha = TRUE) / 255
    rgba[4L, ] <- as.double(!is.na(colour))
    rgba
  })
}

raster_scene_3d <- function(packet, camera, viewport, antialias = 2L, light = NULL,
                           id_pass = FALSE) {
  validate_camera_3d(camera)
  scene_assert(isTRUE(camera$clip), "The raster backend requires clip = 'on'; an extended raster canvas is not implemented.")
  scene_assert(all(packet$polylines$hidden == 0L), "The raster backend requires hidden = 'hide'; hidden-line overrides are not implemented.")
  scene_assert(!any(packet$polylines$arrow_start | packet$polylines$arrow_end),
    "The raster backend does not implement arrowheads.")
  if (!id_pass) {
    face_styles <- packet$styles[match(unique(packet$faces$style_id), packet$styles$style_id), , drop = FALSE]
    scene_assert(all(is.na(style_value_3d(face_styles, "colour", NA_character_))),
      "The raster backend requires face colour = NA; emit boundary strokes as polylines.")
    marker_styles <- packet$styles[match(unique(packet$markers$style_id), packet$styles$style_id), , drop = FALSE]
    separate_paint <- style_value_3d(marker_styles, "shape", 19) %in% 21:25 &
      style_value_3d(marker_styles, "stroke", 0.5) > 0 &
      !is.na(style_value_3d(marker_styles, "colour", NA_character_))
    scene_assert(!any(separate_paint), "The raster backend does not implement separate marker fill and stroke paints.")
  }
  prepared <- raster_prepare_cpp(packet[names(packet) != "styles"], camera, viewport)
  colours <- raster_colours_3d(packet, prepared, light)
  line <- packet$polylines
  styles <- packet$styles[match(line$style_id, packet$styles$style_id), , drop = FALSE]
  lty <- style_value_3d(styles, "linetype", 1)
  dashes <- lapply(seq_along(lty), function(i) dash_pattern_3d(lty[i], line$width_mm[i]))
  pixels <- ceiling(c(viewport$width_mm, viewport$height_mm) * viewport$dpi / 25.4)
  scene_assert(all(is.finite(pixels)) && all(pixels <= .Machine$integer.max), "Unrepresentable raster dimensions.")
  result <- raster_draw_cpp(prepared$pointer, colours, dashes,
    as.integer(pixels[1L]), as.integer(pixels[2L]), as.integer(antialias), id_pass)
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
    grid::textGrob(text_label_3d(styles), text$x[i], text$y[i],
      hjust = tab$hjust[row], vjust = tab$vjust[row], rot = tab$angle[row],
      gp = material_gp_3d(styles, tab$opacity[row], 4L))
  })
}

render_raster_panel_3d <- function(x, furniture, backend) {
  scene <- scene_compile(furniture$packet, furniture$camera, furniture$viewport)
  part <- function(partition) {
    selected <- scene
    selected$draw_order <- scene$draw_order[scene$fragments$partition[scene$draw_order] %in% partition]
    render_scene_grid(selected, furniture$viewport)
  }
  packet <- merge_scene_packets(x$packets, x$panel_params$panel_id)
  result <- raster_scene_3d(packet, furniture$camera, furniture$viewport,
    backend$antialias, x$panel_params$coord_params$light)
  image <- grid::rasterGrob(result$image, width = grid::unit(1, "npc"),
    height = grid::unit(1, "npc"), interpolate = FALSE, name = "scene-raster-image-3d")
  children <- c(list(part(c(0L, 1L)), image), raster_text_3d(result, packet), list(part(2L)))
  grob <- grid::gTree(children = do.call(grid::gList, children), raster_result = result,
    name = "scene-raster-3d")
  list(scene = scene, grob = grob)
}
