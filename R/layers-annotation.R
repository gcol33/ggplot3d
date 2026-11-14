# ggplot2 rectangles, tiles, labels and annotation grobs in a 3D scene.
# Without a z mapping they lie at z = 0; set or map z to move them.

emit_rects_3d <- function(data, params, panel_params) {
  data <- data[valid_rows_3d(data, params, c("xmin", "xmax", "ymin", "ymax", "z")), , drop = FALSE]
  n <- nrow(data)
  if (!n) return(new_scene_packet(panel_id = panel_params$panel_id))
  corners <- data[rep(seq_len(n), each = 4L), , drop = FALSE]
  corners$x <- as.vector(rbind(data$xmin, data$xmax, data$xmax, data$xmin))
  corners$y <- as.vector(rbind(data$ymin, data$ymin, data$ymax, data$ymax))
  quad <- matrix(seq_len(4L * n), ncol = 4L, byrow = TRUE)
  # A rectangle of zero width or height, such as an empty histogram bin, has
  # no face; its outline is still drawn.
  filled <- which(!is.na(data$fill) & data$xmax != data$xmin & data$ymax != data$ymin)
  stroked <- which(!is.na(data$colour))
  outline <- cbind(quad[stroked, , drop = FALSE], quad[stroked, 1L])
  packet_from_primitives_3d(corners,
    c(as.vector(t(quad[filled, , drop = FALSE])), as.vector(t(outline))),
    c(rep("faces", length(filled)), rep("polylines", length(stroked))),
    params, panel_params$panel_id, c(rep.int(4L, length(filled)), rep.int(5L, length(stroked))))
}

text_size_pt_3d <- function(size.unit) {
  switch(size.unit %or3d% "mm", mm = 72.27 / 25.4, pt = 1, cm = 72.27 / 2.54, "in" = 72.27, pc = 12,
    stop("Unsupported text size unit.", call. = FALSE))
}

# Padding and corner radius in mm. Units relative to text (lines, char) refer
# to each label's own font size, as in ggplot2; a plain number is in lines.
label_unit_mm_3d <- function(value, fontsize, lineheight) {
  if (is.numeric(value) && !grid::is.unit(value)) value <- grid::unit(value, "lines")
  value <- value[1L]
  type <- grid::unitType(value)
  if (type == "lines") return(as.numeric(value) * fontsize * lineheight * 25.4 / 72.27)
  if (type == "char") return(as.numeric(value) * fontsize * 25.4 / 72.27)
  rep(grid::convertUnit(value, "mm", valueOnly = TRUE), length(fontsize))
}

# Font size in points and line height of each text row.
text_rows_metrics_3d <- function(data, params) {
  list(fontsize = data$size * text_size_pt_3d(params$size.unit),
       lineheight = data$lineheight %or3d% rep(1.2, nrow(data)))
}

# The style columns of a label box: padding, corner radius, border colour, and
# the text colour when it differs from the border.
label_box_columns_3d <- function(data, params) {
  metrics <- text_rows_metrics_3d(data, params)
  data$label_padding <- label_unit_mm_3d(params$label.padding %or3d% grid::unit(0.25, "lines"), metrics$fontsize, metrics$lineheight)
  data$label_r <- label_unit_mm_3d(params$label.r %or3d% grid::unit(0.15, "lines"), metrics$fontsize, metrics$lineheight)
  data$border_colour <- rep(params$border.colour %or3d% data$colour, length.out = nrow(data))
  if (!is.null(params$text.colour)) data$colour <- rep(params$text.colour, length.out = nrow(data))
  data
}

emit_labels_3d <- function(data, params, panel_params) {
  emit_text_3d(label_box_columns_3d(data, params), params, panel_params)
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

# Overlay grobs that depend on the projection are placed at draw time. The
# context holds the projector from frame positions to panel millimetres, the
# panel size in millimetres, and the state kept across the frames of an
# animation (NULL otherwise).
place_overlay_3d <- function(grob, context) UseMethod("place_overlay_3d")

#' @export
place_overlay_3d.default <- function(grob, context) grob

#' @export
place_overlay_3d.scene_annotation_grob <- function(grob, context) {
  xy <- context$project(grob$corners)[, 1:2, drop = FALSE]
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
#' @param geom Name of a registered geom, such as `"point"` or `"bracket"`,
#'   or a ggplot2 geom prototype.
#' @param x,y,z,xmin,xmax,ymin,ymax,zmin,zmax,xend,yend,zend Positions.
#' @param ... Fixed aesthetics and drawing parameters passed to the 3D layer.
#' @param na.rm Whether to silently remove missing values.
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
  geom <- annotation_geom_3d(geom)
  positions <- Filter(Negate(is.null), list(x = x, y = y, z = z, xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax, zmin = zmin, zmax = zmax, xend = xend, yend = yend, zend = zend))
  common_length_3d(lengths(positions), "position lengths")
  params <- c(list(na.rm = na.rm), list(...))
  aesthetics <- scene_geom_3d(geom, find_emitter_3d(geom))$aesthetics()
  n <- common_length_3d(c(lengths(positions), lengths(params[intersect(names(params), aesthetics)])),
    "parameter lengths")
  data <- data.frame(row.names = seq_len(n))
  for (name in names(positions)) data[[name]] <- rep(positions[[name]], length.out = n)
  make_layer_3d(geom, ggplot2::aes_all(names(data)), data, "identity", "identity", params,
    show.legend = FALSE, inherit.aes = FALSE)
}

annotation_geom_3d <- function(geom) {
  if (is.character(geom) && length(geom) == 1L && !is.na(geom)) {
    classes <- ls(.emitters_3d)
    names <- tolower(sub("3D$", "", sub("^Geom", "", classes)))
    index <- match(tolower(gsub("_", "", geom, fixed = TRUE)), names)
    if (is.na(index)) stop("No 3D emitter registered for geom '", geom, "'.", call. = FALSE)
    geom <- .emitters_3d[[classes[[index]]]]$geom
  }
  if (!inherits(geom, "Geom") || is.null(find_emitter_3d(geom))) {
    stop("geom must be a registered geom name or prototype.", call. = FALSE)
  }
  geom
}

register_geom_3d(ggplot2::GeomRect, emit_rects_3d, tween = "row")
register_geom_3d(ggplot2::GeomLabel, emit_labels_3d,
  parameters = list(label.padding = grid::unit(0.25, "lines"), label.r = grid::unit(0.15, "lines"),
    border.colour = NULL, text.colour = NULL), positions = mark_positions_3d, tween = "row")
register_geom_3d(ggplot2::GeomCustomAnn,
  annotation_emitter_3d(function(params) params$grob),
  parameters = list(grob = NULL, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
  default_z = -Inf)
