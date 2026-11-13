# ggplot2 rectangles, tiles, reference lines, labels and annotation grobs in a
# 3D scene. Without a z mapping they lie at z = 0; set or map z to move them.

emit_rects_3d <- function(data, params, panel_params) {
  data <- data[valid_rows_3d(data, params, c("xmin", "xmax", "ymin", "ymax", "z")), , drop = FALSE]
  n <- nrow(data)
  if (!n) return(new_scene_packet(panel_id = panel_params$panel_id))
  corners <- data[rep(seq_len(n), each = 4L), , drop = FALSE]
  corners$x <- as.vector(rbind(data$xmin, data$xmax, data$xmax, data$xmin))
  corners$y <- as.vector(rbind(data$ymin, data$ymin, data$ymax, data$ymax))
  quad <- matrix(seq_len(4L * n), ncol = 4L, byrow = TRUE)
  filled <- which(!is.na(data$fill)); stroked <- which(!is.na(data$colour))
  outline <- cbind(quad[stroked, , drop = FALSE], quad[stroked, 1L])
  packet_from_primitives_3d(corners,
    c(as.vector(t(quad[filled, , drop = FALSE])), as.vector(t(outline))),
    c(rep("faces", length(filled)), rep("polylines", length(stroked))),
    params, panel_params$panel_id, c(rep.int(4L, length(filled)), rep.int(5L, length(stroked))))
}

# A horizontal or vertical reference line spans the panel box along the other
# planar axis at its height.
reference_line_emitter_3d <- function(axis) {
  along <- setdiff(c("x", "y"), axis)
  column <- paste0(axis, "intercept")
  function(data, params, panel_params) {
    data <- data[valid_rows_3d(data, params, c(column, "z")), , drop = FALSE]
    n <- nrow(data)
    ends <- data[rep(seq_len(n), each = 2L), , drop = FALSE]
    ends[[axis]] <- rep(data[[column]], each = 2L)
    ends[[along]] <- rep(c(0, 1), n)
    packet_from_primitives_3d(ends, seq_len(2L * n), rep("polylines", n), params, panel_params$panel_id, rep.int(2L, n))
  }
}

text_size_pt_3d <- function(size.unit) {
  switch(size.unit %or3d% "mm", mm = 72.27 / 25.4, pt = 1, cm = 72.27 / 2.54, "in" = 72.27, pc = 12,
    stop("Unsupported text size unit.", call. = FALSE))
}

# Padding and corner radius in mm. Units relative to text (lines, char) refer
# to each label's own font size, as in ggplot2.
label_unit_mm_3d <- function(value, fontsize, lineheight) {
  value <- value[1L]
  type <- grid::unitType(value)
  if (type == "lines") return(as.numeric(value) * fontsize * lineheight * 25.4 / 72.27)
  if (type == "char") return(as.numeric(value) * fontsize * 25.4 / 72.27)
  rep(grid::convertUnit(value, "mm", valueOnly = TRUE), length(fontsize))
}

emit_labels_3d <- function(data, params, panel_params) {
  fontsize <- data$size * text_size_pt_3d(params$size.unit)
  lineheight <- data$lineheight %or3d% rep(1.2, nrow(data))
  data$label_padding <- label_unit_mm_3d(params$label.padding %or3d% grid::unit(0.25, "lines"), fontsize, lineheight)
  data$label_r <- label_unit_mm_3d(params$label.r %or3d% grid::unit(0.15, "lines"), fontsize, lineheight)
  data$border_colour <- rep(params$border.colour %or3d% data$colour, length.out = nrow(data))
  if (!is.null(params$text.colour)) data$colour <- rep(params$text.colour, length.out = nrow(data))
  emit_text_3d(data, params, panel_params)
}

# An annotation grob fills the screen rectangle that bounds its x/y rectangle
# on the floor of the panel box. It is drawn over the scene.
annotation_emitter_3d <- function(make_grob) {
  function(data, params, panel_params) {
    box <- data.frame(x = c(params$xmin, params$xmax, params$xmax, params$xmin),
      y = c(params$ymin, params$ymin, params$ymax, params$ymax), z = -Inf)
    box <- normalise_positions_3d(box, panel_params)
    corners <- frame_of_3d(panel_params)$map(as.matrix(box[c("x", "y", "z")]))
    grid::gTree(children = grid::gList(make_grob(params)), corners = corners,
      cl = c("scene_annotation_grob", "scene_overlay_grob"))
  }
}

place_annotation_3d <- function(grob, project) {
  if (!inherits(grob, "scene_annotation_grob")) return(grob)
  xy <- project(grob$corners)[, 1:2, drop = FALSE]
  if (any(!is.finite(xy))) return(grid::nullGrob())
  low <- apply(xy, 2L, min); high <- apply(xy, 2L, max)
  grob$vp <- grid::viewport(grid::unit(low[1L], "mm"), grid::unit(low[2L], "mm"),
    grid::unit(high[1L] - low[1L], "mm"), grid::unit(high[2L] - low[2L], "mm"), just = c(0, 0))
  grob
}

#' Annotations with a height
#'
#' Adds a layer of fixed values like [ggplot2::annotate()], with z positions
#' as well. Every position vector has length one or a common length.
#' @param geom Name of a geom, as in [ggplot2::annotate()].
#' @param x,y,z,xmin,xmax,ymin,ymax,zmin,zmax,xend,yend,zend Positions.
#' @param ...,na.rm Passed to [ggplot2::annotate()].
#' @return A ggplot2 layer.
#' @examples
#' d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
#'   annotate_3d("text", x = 2, y = 2, z = 3, label = "peak") +
#'   annotate_3d("segment", x = 1, y = 1, z = 0, xend = 1, yend = 1, zend = 2) +
#'   coord_3d()
#' @export
annotate_3d <- function(geom, x = NULL, y = NULL, z = NULL, xmin = NULL, xmax = NULL,
                        ymin = NULL, ymax = NULL, zmin = NULL, zmax = NULL,
                        xend = NULL, yend = NULL, zend = NULL, ..., na.rm = FALSE) {
  planar <- Filter(Negate(is.null), list(x = x, y = y, xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax, xend = xend, yend = yend))
  heights <- Filter(Negate(is.null), list(z = z, zmin = zmin, zmax = zmax, zend = zend))
  sizes <- lengths(c(planar, heights))
  n <- max(c(1L, sizes))
  if (any(!sizes %in% c(1L, n))) {
    stop("Unequal position lengths: ", paste0(names(sizes), " (", sizes, ")", collapse = ", "), ".", call. = FALSE)
  }
  planar <- lapply(planar, rep, length.out = n)
  layer <- do.call(ggplot2::annotate, c(list(geom = geom), planar, list(...), list(na.rm = na.rm)))
  if (!length(heights)) return(layer)
  for (name in names(heights)) layer$data[[name]] <- rep(heights[[name]], length.out = nrow(layer$data))
  mapping <- ggplot2::aes_all(names(heights))
  for (name in names(mapping)) layer$mapping[[name]] <- mapping[[name]]
  layer
}

register_geom_3d(ggplot2::GeomRect, emit_rects_3d)
register_geom_3d(ggplot2::GeomHline, reference_line_emitter_3d("y"))
register_geom_3d(ggplot2::GeomVline, reference_line_emitter_3d("x"))
register_geom_3d(ggplot2::GeomLabel, emit_labels_3d,
  parameters = list(label.padding = grid::unit(0.25, "lines"), label.r = grid::unit(0.15, "lines"),
    border.colour = NULL, text.colour = NULL))
register_geom_3d(ggplot2::GeomCustomAnn,
  annotation_emitter_3d(function(params) params$grob),
  parameters = list(grob = NULL, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
  default_z = -Inf)
register_geom_3d(ggplot2::GeomRasterAnn,
  annotation_emitter_3d(function(params) grid::rasterGrob(params$raster, width = grid::unit(1, "npc"),
    height = grid::unit(1, "npc"), interpolate = isTRUE(params$interpolate))),
  parameters = list(raster = NULL, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, interpolate = FALSE),
  default_z = -Inf)
