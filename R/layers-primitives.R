make_layer_3d <- function(geom, mapping, data, stat, position, params, show.legend, inherit.aes,
                          constructor = ggplot2::layer, default_stat = NULL) {
  geom <- resolve_layer_component_3d(geom, "geom")
  stat <- resolve_layer_component_3d(stat, "stat")
  position <- resolve_layer_component_3d(position, "position")
  entry <- find_emitter_3d(geom)
  if (is.null(entry)) stop("No 3D emitter registered for geom ", class(geom)[1L], ".", call. = FALSE)
  entry$stat_adapter <- NULL
  geom <- scene_geom_3d(geom, entry)
  params <- layer_params_3d(params, geom, stat, default_stat)
  parent <- constructor(geom = geom, mapping = mapping, data = data,
              stat = stat, position = position, params = params,
              show.legend = show.legend, inherit.aes = inherit.aes)
  ggplot2::ggproto(NULL, parent,
    compute_statistic = function(self, data, layout) {
      if (inherits(layout$coord, "Coord3D")) check_3d_build(inherits(layout, "Layout3D"))
      ggplot2::ggproto_parent(parent, self)$compute_statistic(data, layout)
    })
}

#' Point, path, segment, text and polygon layers in three dimensions
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard ggplot2 layer arguments.
#' @details Large point clouds: `geom_point_3d(render = "splat")` draws each
#'   point in a raster backend as a sphere of the point's size, depth-tested per
#'   pixel, and darkens points by eye-dome lighting, so an unlit cloud shows its
#'   depth structure. `edl` (default 1, 0 for none) sets the lighting strength.
#'   The vector backend draws splats as ordinary points and refuses panels with
#'   more markers than the option `ggplot3d.vector_markers` (default 50000).
#'
#'   Text in a plane: `geom_text_3d(orientation = "plane", plane = "xy")` lays
#'   each label flat in the plane through its position spanned by two axes
#'   (`"xy"`, `"xz"` or `"yz"`), read along the first axis and turned within
#'   the plane by the `angle` aesthetic. The label is cut from the glyph
#'   outlines of its font (read with the 'systemfonts' package) into faces that
#'   the camera foreshortens and that hide and are hidden by the data; `size`
#'   is the text size at the camera target. Seen from behind the plane, a
#'   label reads in the turned direction instead of mirrored.
#' @param orientation Text layers: `"screen"` draws labels upright on the
#'   page, `"plane"` draws font outline geometry in three dimensions.
#' @param plane Text plane spanned by `"xy"`, `"xz"` or `"yz"`; defaults to
#'   `"xy"` when neither `normal` nor `facing` is supplied.
#' @param normal A finite nonzero numeric triple, the plane normal in
#'   normalized panel coordinates. Supply one of `plane`, `normal` or `facing`.
#' @param up A finite nonzero reading-up hint in normalized panel coordinates.
#'   With `normal`, defaults to the z axis (y for a z normal). Camera-facing
#'   text defaults to the camera's reading-up direction. A parallel hint uses
#'   the least aligned coordinate axis to define the reading direction.
#' @param facing `"camera_plane"` gives labels a shared view-facing normal;
#'   `"camera_point"` faces each anchor toward the perspective eye. Parallel
#'   projections use the reversed sight ray, including oblique shear.
#' @details Normals and up hints refer to positions after scale normalization.
#'   The frame's local Jacobian followed by aspect scaling transforms plane
#'   tangents into displayed model space. Their cross product transforms
#'   normals by the inverse transpose, preserving perpendicularity under
#'   unequal aspect ratios and curved frames. Glyphs remain rigid in the local
#'   tangent plane. `angle` rotates within that plane. Camera-facing geometry
#'   is resolved for each panel at draw time in every static backend and
#'   follows camera movement in the HTML viewer.
#'
#'   Fixed aesthetics accept ggplot2 aliases such as `color`, `pch`, `cex`,
#'   `lty`, `lwd`, `fg` and `bg`. Supply each aesthetic once: passing both
#'   `color` and `colour` is an error. Geom, stat and position strings accept
#'   ggplot2 names and package suffixes such as `"surface_3d"` and
#'   `"nudge_3d"`. Fixed-purpose reference annotations [geom_refline_3d()]
#'   and [geom_abline_3d()] use identity statistics and positioning.
#' @return A ggplot2 layer emitting unprojected scene geometry.
#' @examples
#' d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(1, 2, 4),
#'                 label = letters[1:3])
#' p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
#' p + geom_point_3d(size = 3) + coord_3d()
#' p + geom_path_3d() + coord_3d()
#' \donttest{
#' cloud <-data.frame(x = rnorm(2000), y = rnorm(2000), z = rnorm(2000))
#' ggplot2::ggplot(cloud, ggplot2::aes(x, y, z = z)) +
#'   geom_point_3d(render = "splat", size = 1) +
#'   coord_3d(backend = backend_3d("raster", dpi = 96))
#' p + geom_segment_3d(ggplot2::aes(xend = x, yend = y, zend = 0)) + coord_3d()
#' p + geom_text_3d(ggplot2::aes(label = label)) + coord_3d()
#' if (requireNamespace("systemfonts", quietly = TRUE)) {
#'   p + geom_text_3d(ggplot2::aes(label = label), orientation = "plane", plane = "xy", size = 8) +
#'     coord_3d(projection = "perspective")
#'   p + geom_text_3d(ggplot2::aes(label = label), orientation = "plane",
#'     normal = c(1, 1, 1), up = c(0, 0, 1)) + coord_3d()
#'   p + geom_text_3d(ggplot2::aes(label = label), orientation = "plane",
#'     facing = "camera_point") + coord_3d(projection = "perspective")
#' }
#' p + geom_polygon_3d(fill = "steelblue", alpha = 0.5) + coord_3d()
#' }
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
                          orientation = "screen", plane = NULL, normal = NULL, up = NULL, facing = NULL,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  geom <- switch(match.arg(orientation, c("screen", "plane")), screen = ggplot2::GeomText, plane = GeomTextPlane3D)
  spec <- Filter(Negate(is.null), list(plane = plane, normal = normal, up = up, facing = facing))
  make_layer_3d(geom, mapping, data, stat, position, c(list(na.rm = na.rm), spec, list(...)), show.legend, inherit.aes)
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
#'   coordinate system's planes for this layer, or `cast_shadow = FALSE`,
#'   which keeps the layer out of the shadows of [shadow_3d()].
#'   Aesthetic aliases follow [geom_point_3d()]; supplying both spellings is
#'   an error. Crossbar `middle.color`/`middle.colour` and
#'   `box.color`/`box.colour` arguments retain their explicit pairs.
#' @param occlusion `"depth"` (default) sorts the layer into the scene by
#'   depth; `"overlay"` draws it over the scene.
#' @param shade Whether to shade faces.
#' @param hidden Strokes behind faces: `"hide"` (default) removes them,
#'   `"dashed"` draws them dashed and `"show"` draws them solid.
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
  params <- layer_params_3d(c(params, list(occlusion = occlusion, shade = shade, hidden = hidden)),
    result$geom, result$stat)
  for (name in names(params)) {
    if (name %in% result$geom$aesthetics()) result$aes_params[[name]] <- params[[name]]
    else {
      if (name %in% result$stat$parameters(TRUE)) result$stat_params[[name]] <- params[[name]]
      if (name %in% c(result$geom$parameters(TRUE), result$geom$extra_params)) result$geom_params[[name]] <- params[[name]]
    }
  }
  result
}
