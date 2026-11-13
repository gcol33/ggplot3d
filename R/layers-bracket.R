GeomBracket3D <- ggplot2::ggproto("GeomBracket3D", ggplot2::Geom,
  required_aes = c("x", "y", "xend", "yend"),
  optional_aes = c("label", "zend"),
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA,
    size = 3.88, family = "", fontface = 1, lineheight = 1.2, hjust = 0.5, vjust = 0.5, angle = 0),
  draw_key = ggplot2::draw_key_path
)

#' Brackets and dimension lines between two points
#'
#' Marks the span from (`x`, `y`, `z`) to (`xend`, `yend`, `zend`) with a line
#' set off to one side and an optional label at its middle. `type = "bracket"`
#' draws a bracket whose tips point back at the two ends, as
#' `ggsignif::geom_signif()` and `ggpubr::geom_bracket()` mark compared
#' groups. `type = "dimension"` draws a dimension line as in technical
#' drawing: extension lines from the two ends, and a line with arrowheads
#' between them.
#'
#' The offset is in units of the panel box, so `offset = c(0, 0, 0.05)` lifts
#' the line by 5% of the box height whatever the z scale. Without a `zend`
#' mapping the span is level at `z`. The label is centred a little past the
#' middle of the line, on the side of the offset: `tip_length` plus 3% of the
#' box. Like other data, parts outside the panel box are clipped;
#' dimension lines drawn outside an object at the edge of the box need
#' `coord_3d(clip_data = FALSE)`.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   layer arguments. `...` takes fixed aesthetics and the 3D drawing
#'   parameters, and `arrow` replaces the arrowheads of dimension lines.
#' @param type `"bracket"` or `"dimension"`.
#' @param offset Displacement of the line from the two ends, a vector
#'   `c(x, y, z)` in fractions of the panel box.
#' @param tip_length Length of the bracket tips, and of the extension lines
#'   beyond the dimension line, in fractions of the panel box. Tips never reach
#'   past the two ends.
#' @return A ggplot2 layer.
#' @section Aesthetics:
#' `x`, `y`, `xend` and `yend` are required; `z`, `zend`, `label`, `colour`,
#' `linewidth`, `linetype`, `alpha` and the text aesthetics `size`, `family`,
#' `fontface`, `lineheight`, `hjust`, `vjust` and `angle` are optional.
#' @examples
#' cols <- data.frame(x = 1:3, y = 1, z = c(4, 7, 5))
#' pair <- data.frame(x = 1, xend = 2, y = 1, yend = 1, z = 7.5, label = "p = 0.01")
#' ggplot2::ggplot(cols, ggplot2::aes(x, y, z = z)) +
#'   geom_col_3d() +
#'   geom_bracket_3d(ggplot2::aes(xend = xend, yend = yend, label = label), data = pair) +
#'   coord_3d()
#'
#' box <- data.frame(x = 0, y = 0, z = 0, xend = 4, yend = 0, zend = 0, label = "4 m")
#' ggplot2::ggplot(box, ggplot2::aes(x, y, z = z, xend = xend, yend = yend, zend = zend, label = label)) +
#'   geom_bracket_3d(type = "dimension", offset = c(0, -0.08, 0)) +
#'   coord_3d(clip_data = FALSE)
#' @export
geom_bracket_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                            type = c("bracket", "dimension"), offset = c(0, 0, 0.05), tip_length = 0.02,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  type <- match.arg(type)
  check_bracket_offset_3d(offset, tip_length)
  make_layer_3d(GeomBracket3D, mapping, data, stat, position,
    c(list(type = type, offset = offset, tip_length = tip_length, na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}

check_bracket_offset_3d <- function(offset, tip_length) {
  if (!is.numeric(offset) || length(offset) != 3L || any(!is.finite(offset)) || all(offset == 0)) {
    stop("offset must be three finite numbers c(x, y, z), not all zero.", call. = FALSE)
  }
  if (!is.numeric(tip_length) || length(tip_length) != 1L || !is.finite(tip_length) || tip_length < 0) {
    stop("tip_length must be a nonnegative number.", call. = FALSE)
  }
}

# Rows of data repeated once per point of each path, with the positions of the
# k points of row i taken from row i of points[[1]], ..., points[[k]].
path_rows_3d <- function(data, points) {
  k <- length(points)
  rows <- data[rep(seq_len(nrow(data)), each = k), , drop = FALSE]
  for (j in seq_len(3L)) rows[[c("x", "y", "z")[j]]] <- as.vector(do.call(rbind, lapply(points, function(p) p[, j])))
  rows
}

emit_paths_of_3d <- function(data, points, params, panel_params) {
  k <- length(points); n <- nrow(data)
  packet_from_primitives_3d(path_rows_3d(data, points), seq_len(k * n), rep("polylines", n), params,
    panel_params$panel_id, rep.int(k, n))
}

emit_brackets_3d <- function(data, params, panel_params) {
  data$zend <- data$zend %or3d% data$z
  data <- data[valid_rows_3d(data, params, c("x", "y", "z", "xend", "yend", "zend")), , drop = FALSE]
  if (!nrow(data)) return(new_scene_packet(panel_id = panel_params$panel_id))
  check_bracket_offset_3d(params$offset, params$tip_length)
  offset <- matrix(params$offset, nrow(data), 3L, byrow = TRUE)
  away <- offset / sqrt(sum(params$offset^2))
  tip <- min(params$tip_length, sqrt(sum(params$offset^2)))
  a <- unname(as.matrix(data[c("x", "y", "z")])); b <- unname(as.matrix(data[c("xend", "yend", "zend")]))
  start <- a + offset; end <- b + offset
  packets <- if (identical(params$type, "dimension")) {
    gap <- tip / 2
    plain <- params; plain$arrow <- NULL
    params$arrow <- params$arrow %or3d% grid::arrow(angle = 20, length = grid::unit(2, "mm"), ends = "both", type = "closed")
    list(emit_paths_of_3d(data, list(a + gap * away, start + tip * away), plain, panel_params),
         emit_paths_of_3d(data, list(b + gap * away, end + tip * away), plain, panel_params),
         emit_paths_of_3d(data, list(start, end), params, panel_params))
  } else {
    list(emit_paths_of_3d(data, list(start - tip * away, start, end, end - tip * away), params, panel_params))
  }
  if (!is.null(data$label)) {
    labels <- data
    middle <- (start + end) / 2 + (tip + 0.03) * away
    labels[c("x", "y", "z")] <- as.data.frame(middle)
    packets[[length(packets) + 1L]] <- emit_text_3d(labels, params, panel_params)
  }
  merge_scene_packets(packets, panel_params$panel_id)
}

register_geom_3d(GeomBracket3D, emit_brackets_3d,
  parameters = list(type = "bracket", offset = c(0, 0, 0.05), tip_length = 0.02))
