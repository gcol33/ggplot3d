vector_norm_3d <- function(data) sqrt(rowSums(as.matrix(data[c("u", "v", "w")])^2))

prepare_vectors_3d <- function(out, data, plot, params) {
  if (!all(c("x", "y", "z", "u", "v", "w") %in% names(out))) return(out)
  out$norm <- vector_norm_3d(out)
  length <- rep(params$scale, nrow(out))
  if (isTRUE(params$normalize)) length <- ifelse(out$norm > 0, length / out$norm, 0)
  pivot <- switch(params$pivot, tail = 0, middle = 0.5, tip = 1)
  for (i in seq_len(3L)) {
    axis <- c("x", "y", "z")[i]
    delta <- out[[c("u", "v", "w")[i]]] * length
    out[[paste0(axis, "end")]] <- out[[axis]] + (1 - pivot) * delta
    out[[axis]] <- out[[axis]] - pivot * delta
  }
  out
}

GeomQuiver3D <- ggplot2::ggproto("GeomQuiver3D", ggplot2::Geom,
  required_aes = c("x", "y", "z", "xend", "yend", "zend"),
  optional_aes = c("u", "v", "w", "norm"),
  default_aes = ggplot2::aes(colour = "grey25", fill = "grey50", linewidth = 0.5, linetype = 1, alpha = 1),
  draw_key = ggplot2::draw_key_path
)

vector_maps_3d <- function(direction, radial, axial) {
  maps <- align_z_3d(direction)
  for (i in seq_len(nrow(direction))) maps[, , i] <- sweep(maps[, , i], 2L, c(radial[i], radial[i], axial[i]), "*")
  maps
}

emit_vectors_3d <- function(data, params, panel_params) {
  frame <- frame_of_3d(panel_params)
  scale <- model_scale_3d(panel_params)
  start <- frame$map(as.matrix(data[c("x", "y", "z")]))
  end <- frame$map(as.matrix(data[c("xend", "yend", "zend")]))
  direction <- sweep(end - start, 2L, scale, "*")
  length <- sqrt(rowSums(direction^2))
  keep <- is.finite(length) & length > 0
  data <- data[keep, , drop = FALSE]
  direction <- direction[keep, , drop = FALSE]; length <- length[keep]
  start <- start[keep, , drop = FALSE]; end <- end[keep, , drop = FALSE]
  if (!nrow(data)) return(new_scene_packet(panel_id = panel_params$panel_id))
  unit_direction <- direction / length
  head <- if (params$head == "cone") length * params$head_length else rep(0, length(length))
  packets <- list()
  if (params$shaft == "line") {
    shaft <- data
    tip <- end - sweep(unit_direction * head, 2L, scale, "/")
    shaft[c("x", "y", "z")] <- as.data.frame(start)
    shaft[c("xend", "yend", "zend")] <- as.data.frame(tip)
    line_params <- params
    if (params$head == "flat") line_params$arrow <- grid::arrow(length = grid::unit(2.5, "mm"), type = "closed")
    packets[[1L]] <- emit_segments_3d(shaft, line_params, panel_params)
  } else if (params$shaft == "tube") {
    mesh <- glyph_mesh_3d("cylinder", params$detail)
    mesh$vertices[, 3L] <- mesh$vertices[, 3L] + 1
    maps <- vector_maps_3d(direction, length * params$shaft_width, (length - head) / 2)
    packets[[1L]] <- emit_glyph_instances_3d(data, maps, rep("model", nrow(data)), mesh, params, panel_params)
  }
  if (params$head == "flat" && params$shaft != "line") {
    arrow <- data
    base <- end - sweep(unit_direction * (length * 0.25), 2L, scale, "/")
    arrow[c("x", "y", "z")] <- as.data.frame(base)
    arrow[c("xend", "yend", "zend")] <- as.data.frame(end)
    line_params <- params
    line_params$arrow <- grid::arrow(length = grid::unit(2.5, "mm"), type = "closed")
    packets[[length(packets) + 1L]] <- emit_segments_3d(arrow, line_params, panel_params)
  }
  if (params$head == "cone" && params$head_length > 0 && params$head_width > 0) {
    mesh <- glyph_mesh_3d("cone", params$detail)
    mesh$vertices[, 3L] <- mesh$vertices[, 3L] - 1
    anchors <- data
    anchors[c("x", "y", "z")] <- anchors[c("xend", "yend", "zend")]
    maps <- vector_maps_3d(direction, length * params$head_width, head / 2)
    packets[[length(packets) + 1L]] <- emit_glyph_instances_3d(anchors, maps, rep("model", nrow(data)), mesh, params, panel_params)
  }
  packet <- merge_emitted_3d(packets, panel_params)
  attr(packet, "model_space") <- TRUE
  packet
}

vector_parameters_3d <- list(scale = 1, normalize = FALSE, pivot = "tail", head = "cone",
  shaft = "line", head_length = 0.25, head_width = 0.1, shaft_width = 0.025, detail = 1L)
register_geom_3d(GeomQuiver3D, emit_vectors_3d,
  prepare_aesthetics = prepare_vectors_3d, parameters = vector_parameters_3d,
  positions = c("nudge", "jitter"), tween = "row",
  extents = list(x = extent_3d("x", "xend"), y = extent_3d("y", "yend"), z = extent_3d("z", "zend")))

vector_layer_3d <- function(mapping, data, stat, position, extra, scale, normalize, pivot, head, shaft,
                            head_length, head_width, shaft_width, detail, shade, na.rm, show.legend, inherit.aes) {
  for (name in c("scale", "head_length", "head_width", "shaft_width")) {
    value <- get(name)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) stop(name, " must be nonnegative.", call. = FALSE)
  }
  if (head_length > 1) stop("head_length must be at most one.", call. = FALSE)
  make_layer_3d(GeomQuiver3D, mapping, data, stat, position,
    stats_params_3d(list(scale = scale, normalize = check_flag_3d(normalize, "normalize"),
      pivot = match.arg(pivot, c("tail", "middle", "tip")), head = match.arg(head, c("cone", "flat", "none")),
      shaft = match.arg(shaft, c("line", "tube", "none")), head_length = head_length,
      head_width = head_width, shaft_width = shaft_width, detail = check_detail_3d(detail),
      shade = shade, na.rm = na.rm), extra), show.legend, inherit.aes)
}

#' Vector fields with solid cone heads
#'
#' Maps x/y/z as anchors and u/v/w as vector components in original data
#' units. Endpoints are transformed by the position scales. Cone orientation
#' is calculated from the resulting endpoints after frame and aspect mapping.
#' `normalize = TRUE` makes the input vectors unit length before applying
#' `scale`. Computed variables `norm`, `xend`, `yend`, `zend` are available
#' through `after_stat()`. Zero-length vectors draw no geometry.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param scale Nonnegative vector length multiplier.
#' @param normalize Whether to normalize the input vectors.
#' @param pivot Anchor location: `"tail"`, `"middle"` or `"tip"`.
#' @param head Head type: solid `"cone"`, a `"flat"` arrow, or `"none"`.
#' @param shaft Shaft geometry: `"line"`, `"tube"` or `"none"`.
#' @param head_length,head_width Head length and base radius as fractions of vector length.
#' @param shaft_width Tube radius as a fraction of vector length.
#' @param detail Glyph mesh subdivision, from 0 to 6.
#' @param shade Whether to apply lighting.
#' @return A ggplot2 layer.
#' @examples
#' d <- expand.grid(x = -1:1, y = -1:1, z = 0:1)
#' d$u <- -d$y; d$v <- d$x; d$w <- 0.5
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w,
#'                                fill = ggplot2::after_stat(norm))) +
#'   geom_quiver_3d(scale = 0.4) + coord_3d()
#' @export
geom_quiver_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                           scale = 1, normalize = FALSE, pivot = "tail", head = "cone", shaft = "line",
                           head_length = 0.25, head_width = 0.1, shaft_width = 0.025, detail = 1L,
                           shade = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  vector_layer_3d(mapping, data, stat, position, list(...), scale, normalize, pivot, head, shaft,
    head_length, head_width, shaft_width, detail, shade, na.rm, show.legend, inherit.aes)
}

#' @rdname geom_quiver_3d
#' @export
geom_cone_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                         scale = 1, normalize = FALSE, pivot = "tail", head_width = 0.25, detail = 1L,
                         shade = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  vector_layer_3d(mapping, data, stat, position, list(...), scale, normalize, pivot, "cone", "none",
    1, head_width, 0, detail, shade, na.rm, show.legend, inherit.aes)
}
