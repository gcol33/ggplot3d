GeomArc3D <- ggplot2::ggproto("GeomArc3D", ggplot2::Geom,
  required_aes = c("x", "y", "xend", "yend"),
  optional_aes = c("z", "zend", "height"),
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
  draw_key = ggplot2::draw_key_path,
  setup_data = function(data, params) {
    if (is.null(data$zend)) data$zend <- data$z
    data
  }
)

#' Flow arcs between two points
#'
#' Draws a parabolic arc from (`x`, `y`, `z`) to (`xend`, `yend`, `zend`), the
#' flow-map arcs of deck.gl's `ArcLayer`. Arcs rise above their chord by
#' `height` times the chord's horizontal length as drawn in the box (with the
#' `aspect` of [coord_3d()]), so long flows arch higher than short ones and the
#' shape does not depend on the x/y units. Arcs that rise above the box are
#' clipped with the data unless `coord_3d(clip_data = FALSE)`. Without z, arcs start and end on the floor of the
#' box, over a map drawn with [geom_sf_3d()]. `height` can also be mapped.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param height Peak rise as a fraction of the horizontal chord length.
#'   Setting it overrides a mapped `height`.
#' @param n Number of straight pieces per arc.
#' @param arrow Arrow specification from [grid::arrow()].
#' @return A ggplot2 layer.
#' @examples
#' flows <- data.frame(x = 0, y = 0, xend = c(4, -3, 1), yend = c(1, 2, -4),
#'   volume = c(3, 1, 2))
#' ggplot2::ggplot(flows, ggplot2::aes(x, y, xend = xend, yend = yend)) +
#'   geom_arc_3d(ggplot2::aes(colour = volume, linewidth = volume)) +
#'   coord_3d()
#' @export
geom_arc_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                        height = 0.25, n = 32L, arrow = NULL,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!is.numeric(height) || length(height) != 1L || !is.finite(height)) stop("height must be a finite number.", call. = FALSE)
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 1 || n != trunc(n)) stop("n must be a positive integer.", call. = FALSE)
  params <- list(n = as.integer(n), arrow = arrow, na.rm = na.rm)
  if (!missing(height)) params$height <- height
  make_layer_3d(GeomArc3D, mapping, data, stat, position, c(params, list(...)), show.legend, inherit.aes)
}

emit_arcs_3d <- function(data, params, panel_params) {
  data <- data[valid_rows_3d(data, params, c("x", "y", "z", "xend", "yend", "zend")), , drop = FALSE]
  if (!nrow(data)) return(new_scene_packet(panel_id = panel_params$panel_id))
  t <- seq(0, 1, length.out = params$n + 1L)
  rows <- rep(seq_len(nrow(data)), each = length(t))
  s <- rep.int(t, nrow(data))
  out <- data[rows, , drop = FALSE]
  for (axis in c("x", "y", "z")) out[[axis]] <- (1 - s) * data[[axis]][rows] + s * data[[paste0(axis, "end")]][rows]
  aspect <- panel_params$camera$aspect %or3d% c(1, 1, 1)
  chord <- sqrt(((data$xend - data$x) * aspect[1L])^2 + ((data$yend - data$y) * aspect[2L])^2) / aspect[3L]
  rise <- data$height %or3d% rep(params$height, nrow(data))
  out$z <- out$z + 4 * s * (1 - s) * rise[rows] * chord[rows]
  out$group <- rows
  emit_paths_3d(out, params, panel_params)
}

register_geom_3d(GeomArc3D, emit_arcs_3d, parameters = list(height = 0.25, n = 32L), default_z = -Inf)
