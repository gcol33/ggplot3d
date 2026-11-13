make_layer_3d <- function(geom, mapping, data, stat, position, params, show.legend, inherit.aes) {
  entry <- find_emitter_3d(geom)
  known <- c(scene_geom_3d(geom, entry)$aesthetics(), scene_geom_3d(geom, entry)$parameters(TRUE),
             scene_geom_3d(geom, entry)$extra_params,
             if (inherits(stat, "Stat")) stat$parameters(TRUE))
  unknown <- setdiff(names(params), known)
  if (length(unknown)) stop("Unknown 3D layer arguments: ", paste(unknown, collapse = ", "), call. = FALSE)
  result <- ggplot2::layer(geom = scene_geom_3d(geom, entry), mapping = mapping, data = data,
                stat = stat, position = position, params = params,
                show.legend = show.legend, inherit.aes = inherit.aes)
  parent <- result
  ggplot2::ggproto(NULL, result,
    layer_data = function(self, plot_data) {
      data <- ggplot2::ggproto_parent(parent, self)$layer_data(plot_data)
      if (!is.null(entry$prepare_data)) data <- entry$prepare_data(data)
      data
    },
    compute_aesthetics = function(self, data, plot) {
      out <- ggplot2::ggproto_parent(parent, self)$compute_aesthetics(data, plot)
      if (!is.null(entry$prepare_aesthetics)) out <- entry$prepare_aesthetics(out, data, plot, params)
      out
    }
  )
}

#' Point, path, segment, text and polygon layers in three dimensions
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard ggplot2 layer arguments.
#' @details Large point clouds: `geom_point_3d(render = "splat")` draws each
#'   point in a raster backend as a sphere of the point's size, depth-tested per
#'   pixel, and darkens points by eye-dome lighting, so an unlit cloud shows its
#'   depth structure. `edl` (default 1, 0 for none) sets the lighting strength.
#'   The vector backend draws splats as ordinary points and refuses panels with
#'   more markers than the option `ggplot3d.vector_markers` (default 50000).
#' @return A ggplot2 layer emitting unprojected scene geometry.
#' @examples
#' d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(1, 2, 4),
#'                 label = letters[1:3])
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
#' p + geom_point_3d(size = 3) + coord_3d()
#' cloud <- data.frame(x = rnorm(2000), y = rnorm(2000), z = rnorm(2000))
#' ggplot2::ggplot(cloud, ggplot2::aes(x, y, z = z)) +
#'   geom_point_3d(render = "splat", size = 1) +
#'   coord_3d(backend = backend_3d("raster", dpi = 96))
#' p + geom_path_3d() + coord_3d()
#' p + geom_segment_3d(ggplot2::aes(xend = x, yend = y, zend = 0)) + coord_3d()
#' p + geom_text_3d(ggplot2::aes(label = label)) + coord_3d()
#' p + geom_polygon_3d(fill = "steelblue", alpha = 0.5) + coord_3d()
#' @export
geom_point_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(ggplot2::GeomPoint, mapping, data, stat, position, c(list(na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_point_3d
#' @export
geom_path_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(ggplot2::GeomPath, mapping, data, stat, position, c(list(na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_point_3d
#' @export
geom_segment_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(ggplot2::GeomSegment, mapping, data, stat, position, c(list(na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_point_3d
#' @export
geom_text_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(ggplot2::GeomText, mapping, data, stat, position, c(list(na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_point_3d
#' @export
geom_polygon_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(ggplot2::GeomPolygon, mapping, data, stat, position, c(list(na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' Adapt a registered ggplot2 layer to three dimensions
#' @param layer A ggplot2 layer.
#' @param mapping Additional aesthetic mappings.
#' @param ... Fixed aesthetics and rendering parameters, such as
#'   `clip_planes`, a list of [clip_plane_3d()] planes that replaces the
#'   coordinate system's planes for this layer.
#' @param occlusion Depth testing or overlay drawing.
#' @param shade Whether to shade faces.
#' @param hidden Hidden stroke policy.
#' @return A ggplot2 layer adapted to the 3D scene.
#' @examples
#' d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   layer_3d(ggplot2::geom_point(size = 3)) + coord_3d()
#' @export
layer_3d <- function(layer, mapping = NULL, ..., occlusion = "depth", shade = FALSE, hidden = "hide") {
  entry <- find_emitter_3d(layer$geom)
  if (is.null(entry)) stop("No 3D emitter registered for ", class(layer$geom)[1L], call. = FALSE)
  result <- ggplot2::ggproto(NULL, layer, geom = scene_geom_3d(layer$geom, entry))
  result$mapping <- layer$mapping %or3d% ggplot2::aes()
  for (name in names(mapping)) result$mapping[[name]] <- mapping[[name]]
  params <- list(...)
  known <- c(result$geom$aesthetics(), result$geom$parameters(TRUE), layer$stat$parameters(TRUE))
  unknown <- setdiff(names(params), known)
  if (length(unknown)) stop("Unknown layer_3d arguments: ", paste(unknown, collapse = ", "), call. = FALSE)
  for (name in names(params)) {
    if (name %in% result$geom$aesthetics()) result$aes_params[[name]] <- params[[name]]
    else result$geom_params[[name]] <- params[[name]]
  }
  result$geom_params$occlusion <- match.arg(occlusion, c("depth", "overlay"))
  result$geom_params$shade <- shade
  result$geom_params$hidden <- match.arg(hidden, c("hide", "dashed", "show"))
  result
}
