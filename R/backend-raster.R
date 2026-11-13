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
    rgba <- colour_channels_3d(colour, alpha = TRUE) / 255
    rgba[4L, ] <- as.double(!is.na(colour))
    border <- style_value_3d(styles, "colour", NA_character_)
    outline <- colour_channels_3d(border, alpha = TRUE) / 255
    outline[4L, ] <- as.double(!is.na(border))
    attr(rgba, "outline") <- outline
    metrics <- paint_metrics_3d(styles)
    attr(rgba, "width_mm") <- if (kind == "markers") metrics$border_mm * 2 else metrics$width_mm
    attr(rgba, "separate") <- if (kind == "markers") style_value_3d(styles, "shape", 19) %in% 21:25 else rep(FALSE, nrow(styles))
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

raster_scene_3d <- function(packet, camera, viewport, antialias = 2L, light = NULL,
                           id_pass = FALSE) {
  validate_camera_3d(camera)
  prepared <- raster_prepare_cpp(packet[names(packet) != "styles"], camera, viewport)
  colours <- raster_colours_3d(packet, prepared, light)
  line <- packet$polylines
  styles <- packet$styles[match(line$style_id, packet$styles$style_id), , drop = FALSE]
  lty <- style_value_3d(styles, "linetype", 1)
  dashes <- lapply(seq_along(lty), function(i) dash_pattern_3d(lty[i], line$width_mm[i]))
  attr(dashes, "hidden") <- lapply(line$width_mm, function(width) dash_pattern_3d("dashed", width))
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
  extent <- result$extent
  image <- grid::rasterGrob(result$image, x = mean(extent[c(1L, 3L)]), y = mean(extent[c(2L, 4L)]),
    width = grid::unit(extent[3L] - extent[1L], "npc"),
    height = grid::unit(extent[4L] - extent[2L], "npc"), interpolate = FALSE, name = "scene-raster-image-3d")
  children <- c(list(part(c(0L, 1L)), image), raster_text_3d(result, packet), list(part(2L)))
  grob <- grid::gTree(children = do.call(grid::gList, children), raster_result = result,
    name = "scene-raster-3d")
  list(scene = scene, grob = grob)
}
