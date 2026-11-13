prepare_band_3d <- function(out, data, plot, params) {
  if (is.null(out$xend)) out$xend <- out$x %or3d% params$x
  if (is.null(out$yend)) out$yend <- out$y %or3d% params$y
  if (is.null(out[["z"]])) out$z <- out$zmin %or3d% params$zmin
  if (is.null(out$zend)) out$zend <- out$zmax %or3d% params$zmax
  out
}

GeomBand3D <- ggplot2::ggproto("GeomBand3D", ggplot2::Geom,
  required_aes = c("x", "y", "z", "xend", "yend", "zend"),
  optional_aes = c("zmin", "zmax"), default_aes = solid_defaults_3d(),
  draw_key = ggplot2::draw_key_polygon
)

#' Draw a three-dimensional ribbon or wall
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param shade Whether to apply lighting.
#' @details Map x/y and zmin/zmax for a vertical wall, or map both
#'   x/y/z and xend/yend/zend boundaries for a general ruled band.
#'   Input order is retained within groups. Missing endpoints break the band.
#' @return A ggplot2 layer.
#' @examples
#' d <- data.frame(x = seq(0, 6, length.out = 30), y = 0)
#' d$low <- sin(d$x); d$high <- d$low + 0.5
#' ggplot2::ggplot(d, ggplot2::aes(x, y, zmin = low, zmax = high)) +
#'   geom_band_3d(fill = "steelblue") + coord_3d()
#' @export
geom_band_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                         shade = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomBand3D, mapping, data, stat, position,
    c(list(shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

# Outlines: "full" closes the band, "both" strokes the lower and upper
# boundaries, "lower" and "upper" one of them.
emit_bands_3d <- function(data, params, panel_params, outline = "full") {
  valid <- valid_rows_3d(data, params, c("x", "y", "z", "xend", "yend", "zend"))
  rows <- order(match(data$group, unique(data$group)), seq_len(nrow(data)))
  a <- head(rows, -1L); b <- tail(rows, -1L)
  good <- data$group[a] == data$group[b] & valid[a] & valid[b]
  a <- a[good]; b <- b[good]
  n <- nrow(data); upper <- data
  upper[c("x", "y", "z")] <- data[c("xend", "yend", "zend")]
  all <- rbind(data, upper)
  faces <- rbind(cbind(a, b, b + n), cbind(a, b + n, a + n))
  faces <- faces[nondegenerate_triangles_3d(all, faces), , drop = FALSE]
  ends <- unique(c(a[!a %in% b], b[!b %in% a]))
  edges <- rbind(if (outline != "upper") cbind(a, b), if (outline != "lower") cbind(a + n, b + n),
                 if (outline == "full") cbind(ends, ends + n))
  packet_from_faces_edges_3d(all, faces, edges %or3d% matrix(integer(), 0L, 2L), params, panel_params)
}

# A ggplot2 ribbon or area is a flat band between its lower and upper
# boundary at its height; flipped ribbons run along y.
emit_ribbons_3d <- function(data, params, panel_params) {
  flipped <- isTRUE(params$flipped_aes) || isTRUE(data$flipped_aes[1L])
  data <- ggplot2::flip_data(data, flipped)
  data <- data[order(match(data$group, unique(data$group)), data$x), , drop = FALSE]
  band <- data
  band$y <- data$ymin; band$xend <- data$x; band$yend <- data$ymax; band$zend <- data$z
  if (flipped) {
    band[c("x", "y", "xend", "yend")] <- band[c("y", "x", "yend", "xend")]
  }
  outline <- match.arg(params$outline.type %or3d% "both", c("both", "upper", "lower", "full"))
  emit_bands_3d(band, params, panel_params, outline)
}

register_geom_3d(GeomBand3D, emit_bands_3d, prepare_aesthetics = prepare_band_3d,
  positions = c("nudge", "stack"),
  extents = list(x = extent_3d("x", "xend"), y = extent_3d("y", "yend"), z = extent_3d("z", "zend")))

GeomDropline3D <- ggplot2::ggproto("GeomDropline3D", ggplot2::GeomSegment,
  required_aes = c("x", "y", "z")
)

#' Drop segments from points to a box face
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param face Box face: zmin (floor), zmax, xmin, xmax, ymin or ymax.
#' @return A ggplot2 layer.
#' @examples
#' d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_dropline_3d() + geom_point_3d() + coord_3d()
#' @export
geom_dropline_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                             face = "zmin", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  face <- match.arg(face, c("zmin", "zmax", "xmin", "xmax", "ymin", "ymax"))
  make_layer_3d(GeomDropline3D, mapping, data, stat, position,
    c(list(face = face, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

emit_droplines_3d <- function(data, params, panel_params) {
  for (axis in c("x", "y", "z")) data[[paste0(axis, "end")]] <- data[[axis]]
  data[[paste0(substr(params$face, 1L, 1L), "end")]] <- if (endsWith(params$face, "min")) 0 else 1
  emit_segments_3d(data, params, panel_params)
}

register_geom_3d(ggplot2::GeomRibbon, emit_ribbons_3d,
  parameters = list(flipped_aes = FALSE, outline.type = "both"))

register_geom_3d(GeomDropline3D, emit_droplines_3d, positions = c("nudge", "jitter", "dodge", "stack"))
