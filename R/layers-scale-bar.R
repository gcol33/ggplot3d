GeomScaleBar3D <- ggplot2::ggproto("GeomScaleBar3D", ggplot2::GeomBlank,
  default_aes = ggplot2::aes(colour = "black", linewidth = 0.6, alpha = NA,
    size = 3, family = "", fontface = 1, lineheight = 1.2))

#' A scale bar lying in a coordinate plane
#'
#' Draws a bar of the given length in original data units, with in-plane end
#' ticks and a label at the same depth. Perspective foreshortens it together
#' with that plane. It measures distance locally at its own position.
#' The annotation uses the trained scales and leaves their ranges unchanged.
#' @param axis Axis along which the bar measures distance: `"x"`, `"y"`, `"z"`.
#' @param length Positive bar length in original data units.
#' @param unit Text appended to the length.
#' @param plane `"floor"`, a box pane name, `"xy"`, `"xz"`, `"yz"`, or a
#'   `list(axis = , at = )` plane as in [position_project_3d()].
#' @param at Optional named numeric position in original data units. Missing
#'   coordinates start one tenth of the way across the trained panel range;
#'   the plane fixes the coordinate perpendicular to it.
#' @param label Label text, defaulting to the length and unit.
#' @param ... Fixed appearance such as `colour`, `linewidth` and text `size`.
#' @return A ggplot2 layer.
#' @examples
#' d <- data.frame(x = c(0, 1000), y = c(0, 1000), z = c(0, 100))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
#'   annotation_scale_3d(length = 100, unit = "m") +
#'   coord_3d(projection = "perspective")
#' @export
annotation_scale_3d <- function(axis = "x", length = 1, unit = "", plane = "floor", at = NULL,
                                label = NULL, ...) {
  axis <- match.arg(axis, c("x", "y", "z"))
  if (!is.numeric(length) || length(length) != 1L || !is.finite(length) || length <= 0) stop("length must be positive and finite.", call. = FALSE)
  if (!is.character(unit) || length(unit) != 1L || is.na(unit)) stop("unit must be one string.", call. = FALSE)
  if (!is.null(at) && (!is.numeric(at) || is.null(names(at)) || anyDuplicated(names(at)) ||
      !all(names(at) %in% c("x", "y", "z")) || any(!is.finite(at)))) stop("at must be a named finite x/y/z position.", call. = FALSE)
  if (is.null(label)) label <- trimws(paste(format(length, trim = TRUE), unit))
  if (!is.character(label) || length(label) != 1L || is.na(label)) stop("label must be one string.", call. = FALSE)
  if (is.character(plane) && length(plane) == 1L && plane %in% text_planes_3d) {
    plane <- paste0(setdiff(c("x", "y", "z"), strsplit(plane, "", fixed = TRUE)[[1L]]), "min")
  }
  targets <- project_spec_3d(plane)
  make_layer_3d(GeomScaleBar3D, NULL, data.frame(.annotation = 1), "identity", "identity",
    c(list(axis = axis, bar_length = length, unit = unit, targets = targets, at = at, label = label), list(...)), FALSE, FALSE)
}

emit_scale_bar_3d <- function(data, params, panel_params) {
  axes <- c("x", "y", "z"); a <- match(params$axis, axes)
  view <- panel_params[[params$axis]]
  if (isTRUE(view$is_discrete())) stop("A scale bar needs a continuous axis.", call. = FALSE)
  targets <- projection_targets_3d(list(onto = params$targets, axes = NULL), panel_params)
  if (length(targets) != 1L || targets[[1L]]$axis == a) stop("Choose one plane containing the scale-bar axis.", call. = FALSE)
  target <- targets[[1L]]; b <- setdiff(1:3, c(a, target$axis))
  start <- stats::setNames(rep(0.1, 3L), axes)
  for (axis in names(params$at)) start[[axis]] <- scale_unit_3d(axis, params$at[[axis]], panel_params)
  start[target$axis] <- target$value
  raw <- view$get_transformation()$inverse(panel_params$ranges[[params$axis]][1L] + start[a] * diff(panel_params$ranges[[params$axis]]))
  end <- start; end[a] <- scale_unit_3d(params$axis, raw + params$bar_length, panel_params)
  if (any(!is.finite(c(start, end))) || any(c(start, end) < 0 | c(start, end) > 1)) {
    stop("The complete scale bar must fit inside the panel; reduce length or change at.", call. = FALSE)
  }
  vertices <- rbind(start, end, start, start, end, end)
  vertices[c(3, 5), b] <- vertices[c(3, 5), b] - 0.008
  vertices[c(4, 6), b] <- vertices[c(4, 6), b] + 0.008
  rows <- data[rep(1L, 6L), , drop = FALSE]
  rows[axes] <- as.data.frame(vertices)
  line <- packet_from_primitives_3d(rows, 1:6, rep("polylines", 3L), params, panel_params$panel_id, rep.int(2L, 3L))
  text <- data[1L, , drop = FALSE]
  text[axes] <- as.list((start + end) / 2)
  text[[axes[b]]] <- text[[axes[b]]] + if (start[b] > 0.9) -0.025 else 0.025
  text$label <- params$label; text$angle <- 0; text$hjust <- 0.5; text$vjust <- 0.5
  merge_scene_packets(list(line, emit_text_3d(text, params, panel_params)), panel_params$panel_id)
}

register_geom_3d(GeomScaleBar3D, emit_scale_bar_3d, default_z = -Inf,
  parameters = list(axis = "x", bar_length = 1, unit = "", targets = list(list(pane = "floor")), at = NULL, label = "1"))
