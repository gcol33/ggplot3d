GeomRug3D <- ggplot2::ggproto("GeomRug3D", ggplot2::Geom,
  optional_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
  draw_key = ggplot2::draw_key_path
)

parse_rug_sides_3d <- function(sides) {
  if (!is.character(sides) || !length(sides) || anyNA(sides)) stop("sides must be a character vector such as \"floor:x\".", call. = FALSE)
  lapply(strsplit(sides, ":", fixed = TRUE), function(parts) {
    valid <- length(parts) %in% 2:3 && nzchar(parts[1L]) && parts[2L] %in% c("x", "y", "z") &&
      (length(parts) == 2L || grepl("^[xyz](min|max)$", parts[3L]))
    if (!valid) stop("Each rug side is \"pane:axis\" or \"pane:axis:edge\", such as \"floor:x\" or \"back:z:xmax\".", call. = FALSE)
    list(pane = parts[1L], axis = parts[2L], edge = if (length(parts) == 3L) parts[3L])
  })
}

#' Rug ticks on the walls of the panel box
#'
#' Marks the values of one position on a pane of the panel box with short
#' ticks, the three-dimensional counterpart of [ggplot2::geom_rug()].
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param sides Character vector of `"pane:axis"` entries. The pane is
#'   `"floor"`, `"back"` or a pane name as in [position_project_3d()]; the axis
#'   is the position whose values are marked. `"floor:x"` ticks the x values
#'   on the floor, `"back:z"` the z values on the back walls. An optional third
#'   part names the pane edge the ticks start from, such as `"floor:x:ymax"`.
#' @param length Tick length as a fraction of the pane's extent.
#' @details Ticks lie in the pane, perpendicular to the marked axis. Without an
#'   edge they start at the pane edge on the camera side, where the axis labels
#'   usually sit. Like [position_project_3d()], ticks are painted with their
#'   pane, and an axis the layer does not map draws no ticks.
#' @return A ggplot2 layer.
#' @examples
#' d <- data.frame(x = rnorm(80), y = rnorm(80), z = rnorm(80))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_point_3d() +
#'   geom_rug_3d(sides = c("floor:x", "floor:y", "back:z"), alpha = 0.5) +
#'   coord_3d()
#' @export
geom_rug_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                        sides = c("floor:x", "floor:y"), length = 0.03,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  parse_rug_sides_3d(sides)
  if (!is.numeric(length) || length(length) != 1L || !is.finite(length) || length <= 0 || length > 1) {
    stop("length must be a number in (0, 1].", call. = FALSE)
  }
  make_layer_3d(GeomRug3D, mapping, data, stat, position,
    c(list(sides = sides, length = length, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

# Ticks start on the edge whose adjacent pane faces the camera.
rug_edge_3d <- function(axis, partitions, panel_params) {
  planes <- Filter(Negate(is.null), lapply(frame_of_3d(panel_params)$panes(),
    function(pane) tryCatch(pane_plane_3d(pane), error = function(e) NULL)))
  for (plane in planes) {
    if (plane$axis == axis && partitions[[plane$pane$name]] == 2L) return(plane$value)
  }
  0
}

emit_rugs_3d <- function(data, params, panel_params) {
  axes <- c("x", "y", "z")
  partitions <- pane_partitions_3d(panel_params)
  packets <- list()
  for (side in parse_rug_sides_3d(params$sides)) {
    a <- match(side$axis, axes)
    rows <- which(is.finite(data[[side$axis]]))
    if (!length(rows)) next
    for (target in resolve_panes_3d(side$pane, panel_params)) {
      if (target$axis == a) {
        if (side$pane == "back") next
        stop("Rug side ", side$pane, ":", side$axis, " marks the axis the pane holds constant.", call. = FALSE)
      }
      b <- setdiff(seq_len(3L), c(a, target$axis))
      edge <- if (is.null(side$edge)) rug_edge_3d(b, partitions, panel_params) else {
        if (substr(side$edge, 1L, 1L) != axes[b]) stop("Rug edge ", side$edge, " is not an edge of ", target$pane$name, " across the ", side$axis, " axis.", call. = FALSE)
        as.double(endsWith(side$edge, "max"))
      }
      ticks <- data[rows, , drop = FALSE]
      ticks[[axes[target$axis]]] <- target$value
      ticks[[axes[b]]] <- edge
      for (k in seq_len(3L)) ticks[[paste0(axes[k], "end")]] <- ticks[[axes[k]]]
      ticks[[paste0(axes[b], "end")]] <- edge + (1 - 2 * edge) * params$length
      packet <- emit_segments_3d(ticks, params, panel_params)
      packets[[length(packets) + 1L]] <- pane_partition_packet_3d(packet, target$pane, panel_params)
    }
  }
  merge_scene_packets(packets, panel_params$panel_id)
}

# An axis the layer does not map is missing, not a default height.
prepare_rug_3d <- function(out, data, plot, params) {
  for (axis in c("x", "y", "z")) if (is.null(out[[axis]])) out[[axis]] <- rep(NA_real_, nrow(out))
  out
}

register_geom_3d(GeomRug3D, emit_rugs_3d, prepare_aesthetics = prepare_rug_3d,
  parameters = list(sides = c("floor:x", "floor:y"), length = 0.03))
