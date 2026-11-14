#' @include layers-plot-plane.R
NULL

# Images on planes of the panel: one textured quad per image, spanning
# u0..u1 x v0..v1 of the horizontal plane at w and turned onto the plane's
# axes, so it depth-sorts with the scene. pgfplots places an image on a 3D
# plane the same way (\addplot3 graphics), and matplotlib's imshow3d gallery
# example draws an image as a textured surface.

# Packet of one image quad: corners (u0, v0), (u1, v0), (u1, v1), (u0, v1)
# take texture coordinates (0, 0), (1, 0), (1, 1), (0, 1).
texture_quad_packet_3d <- function(texture, u, v, w, axes, data, params, panel_params, alpha = 1) {
  quad <- data.frame(x = u[c(1L, 2L, 2L, 1L)], y = v[c(1L, 1L, 2L, 2L)], z = rep(w, 4L),
    .texture_u = c(0, 1, 1, 0), .texture_v = c(0, 0, 1, 1),
    fill = NA_character_, colour = NA_character_, alpha = alpha, linewidth = 0, linetype = 1,
    .source = rep(data$.source[1L], 4L), .layer = rep(data$.layer[1L], 4L))
  params$texture <- texture_3d(texture, extent = c(0, 1, 0, 1))
  orient_packet_3d(packet_from_primitives_3d(quad, 1:4, "faces", params, panel_params$panel_id, 4L), axes)
}

# Plane axes of an image layer; "floor" is the x/y plane.
texture_plane_axes_3d <- function(plane) {
  if (identical(plane, "floor")) plane <- "xy"
  check_plane_axes_3d(plane)
}

GeomTexturePlane3D <- ggplot2::ggproto("GeomTexturePlane3D", ggplot2::Geom,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(),
  draw_key = ggplot2::draw_key_blank
)

emit_texture_plane_3d <- function(data, params, panel_params) {
  axes <- params$axes
  if (nrow(data) < 2L) return(new_scene_packet(panel_id = panel_params$panel_id))
  texture_quad_packet_3d(params$image, data[[axes[1L]]][1:2], data[[axes[2L]]][1:2], data[[axes[3L]]][1L],
    axes, data, params, panel_params, params$image_alpha)
}

register_geom_3d(GeomTexturePlane3D, emit_texture_plane_3d,
  parameters = list(image = NULL, axes = c("x", "y", "z"), image_alpha = 1))

#' An image on a plane of the panel
#'
#' Lays an image on a plane of the 3D panel, such as a map on the floor or
#' a photograph on a wall. The image is one textured face of the scene: it is
#' foreshortened with the plane and hidden by what lies in front of it, and
#' the raster backend and the [view_3d()] viewer sample it perspective
#' correctly (see [texture_3d()]). Unlike [ggplot2::annotation_raster()], which
#' coord_3d() also draws this way on the floor, the image can stand on any
#' axis plane.
#'
#' The image's horizontal direction runs along the first axis of `plane`
#' from `xmin` to `xmax`, its vertical direction along the second axis from
#' `ymin` to `ymax`; a reversed pair mirrors it. Infinite positions reach the
#' sides of the panel box. The remaining axis is fixed at `at`.
#' @param raster An image accepted by [texture_3d()], or a texture.
#' @param xmin,xmax,ymin,ymax Image extent along the first and second axis of
#'   `plane`.
#' @param plane Two distinct axis letters, such as `"xy"`, `"xz"` or `"yz"`,
#'   or `"floor"` for `"xy"`.
#' @param at Position on the remaining axis; `-Inf` and `Inf` place the image
#'   on the lower and upper box side (0 when no other layer trains that axis).
#' @param interpolate Whether to interpolate between pixels, see
#'   [texture_3d()].
#' @param alpha Opacity of the image.
#' @return A ggplot2 layer.
#' @seealso [geom_raster_3d()] for data on a regular grid, [texture_3d()].
#' @examples
#' logo <- matrix(grDevices::hcl.colors(12, "Zissou 1"), 3, 4)
#' d <- data.frame(x = runif(30), y = runif(30), z = runif(30))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   annotation_raster_3d(logo, 0, 1, 0, 1, plane = "xz", at = Inf, interpolate = FALSE) +
#'   annotation_raster_3d(logo, 0, 1, 0, 1, plane = "floor") +
#'   geom_point_3d() +
#'   coord_3d()
#' @export
annotation_raster_3d <- function(raster, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, plane = "xy",
                                 at = -Inf, interpolate = TRUE, alpha = 1) {
  axes <- texture_plane_axes_3d(plane)
  texture <- texture_3d(raster, interpolate = interpolate)
  extent <- c(xmin, xmax, ymin, ymax)
  if (!is.numeric(extent) || length(extent) != 4L || anyNA(extent) || xmin == xmax || ymin == ymax)
    stop("xmin, xmax, ymin and ymax must be numbers with distinct ends.", call. = FALSE)
  if (!is.numeric(at) || length(at) != 1L || is.na(at)) stop("at must be a number.", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) stop("alpha must be a number between 0 and 1.", call. = FALSE)
  make_layer_3d(GeomTexturePlane3D, NULL, data.frame(.grid = 1), StatPlotPlane3D, "identity",
    list(axes = axes, at = at, extent = as.double(extent), image = texture, image_alpha = alpha),
    FALSE, FALSE)
}

register_geom_3d(ggplot2::GeomRasterAnn, function(data, params, panel_params) {
    texture <- texture_3d(params$raster, interpolate = isTRUE(params$interpolate))
    box <- normalise_positions_3d(data.frame(x = c(params$xmin, params$xmax), y = c(params$ymin, params$ymax), z = -Inf), panel_params)
    texture_quad_packet_3d(texture, box$x, box$y, data$z[1L] %or3d% box$z[1L], c("x", "y", "z"), data, params, panel_params)
  },
  parameters = list(raster = NULL, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, interpolate = FALSE),
  default_z = -Inf)

#' Rasters on planes
#'
#' Draws values on a regular grid as one image lying on a plane of the
#' panel, like [ggplot2::geom_raster()]: each cell is one pixel of a texture
#' (see [texture_3d()]), so a raster of any size is a single face of the
#' scene. The two positions of `plane` locate the cells; the remaining axis
#' takes `at`, or the mapped position on it, and every distinct position on
#' that axis gives its own image, such as one slice of a volume per level.
#' @inheritParams geom_surface_3d
#' @param plane Two distinct axis letters: the axes of the cell positions,
#'   such as `"xy"`, `"xz"` or `"yz"`.
#' @param at Position on the remaining axis, overriding a mapped one; `NULL`
#'   uses the mapped position, or the lower box side when it is not mapped.
#' @param interpolate Whether to interpolate between cells. The default
#'   shows them as sharp squares.
#' @param hjust,vjust Position of each cell's centre within the cell, as in
#'   [ggplot2::geom_raster()].
#' @return A ggplot2 layer.
#' @seealso [annotation_raster_3d()] for an image, [facet_slices_3d()].
#' @examples
#' d <- expand.grid(x = 1:30, y = 1:20)
#' d$value <- with(d, sin(x / 4) * cos(y / 3))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, fill = value)) +
#'   geom_raster_3d() +
#'   coord_3d()
#' slices <- expand.grid(x = 1:25, y = 1:25, z = c(1, 2, 3))
#' slices$value <- with(slices, exp(-((x - 13)^2 + (y - 13)^2) / (20 * z)))
#' ggplot2::ggplot(slices, ggplot2::aes(x, y, z = z, fill = value)) +
#'   geom_raster_3d(alpha = 0.8) +
#'   coord_3d()
#' @export
geom_raster_3d <- function(mapping = NULL, data = NULL, stat = StatRaster3D, position = "identity", ...,
                           plane = "xy", at = NULL, interpolate = FALSE, hjust = 0.5, vjust = 0.5,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  axes <- check_plane_axes_3d(plane)
  if (!is.null(at) && (!is.numeric(at) || length(at) != 1L || is.na(at))) stop("at must be a number or NULL.", call. = FALSE)
  if (!is.logical(interpolate) || length(interpolate) != 1L || is.na(interpolate)) stop("interpolate must be TRUE or FALSE.", call. = FALSE)
  for (just in list(hjust, vjust)) if (!is.numeric(just) || length(just) != 1L || !is.finite(just)) stop("hjust and vjust must be numbers.", call. = FALSE)
  make_layer_3d(GeomRaster3D, mapping, data, stat, position,
    c(list(axes = axes, at = at, interpolate = interpolate, hjust = hjust, vjust = vjust, na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}

StatRaster3D <- ggplot2::ggproto("StatRaster3D", ggplot2::Stat,
  extra_params = c("na.rm", "axes", "at"),
  compute_panel = function(data, scales, axes = c("x", "y", "z"), at = NULL) {
    missing <- setdiff(axes[1:2], names(data))
    if (length(missing)) stop("geom_raster_3d() on plane \"", paste(axes[1:2], collapse = ""),
      "\" needs ", paste(missing, collapse = " and "), " positions.", call. = FALSE)
    if (!is.null(at) || is.null(data[[axes[3L]]])) data[[axes[3L]]] <- plane_at_3d(at %or3d% -Inf, scales, axes[3L])
    data
  }
)

GeomRaster3D <- ggplot2::ggproto("GeomRaster3D", ggplot2::Geom,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(fill = "grey35", alpha = NA),
  draw_key = ggplot2::draw_key_rect
)

emit_raster_3d <- function(data, params, panel_params) {
  axes <- params$axes
  data <- data[valid_rows_3d(data, params), , drop = FALSE]
  if (!nrow(data)) return(new_scene_packet(panel_id = panel_params$panel_id))
  colour <- data$fill
  alpha <- data$alpha %or3d% rep(NA_real_, nrow(data))
  colour[!is.na(colour)] <- scales::alpha(colour[!is.na(colour)], ifelse(is.na(alpha), NA, alpha)[!is.na(colour)])
  packets <- lapply(split(seq_len(nrow(data)), data[[axes[3L]]]), function(rows) {
    a <- data[[axes[1L]]][rows]; b <- data[[axes[2L]]][rows]
    step <- c(ggplot2::resolution(a, FALSE, discrete = TRUE), ggplot2::resolution(b, FALSE, discrete = TRUE))
    i <- round((a - min(a)) / step[1L]) + 1L; j <- round((b - min(b)) / step[2L]) + 1L
    if (any(abs((a - min(a)) / step[1L] + 1 - i) > 1e-6) || any(abs((b - min(b)) / step[2L] + 1 - j) > 1e-6))
      stop("geom_raster_3d() needs cells on a regular grid; use geom_tile() for irregular cells.", call. = FALSE)
    image <- matrix(NA_character_, max(j), max(i))
    image[cbind(max(j) - j + 1L, i)] <- colour[rows]
    image[is.na(image)] <- "transparent"
    u <- c(min(a) - step[1L] * params$hjust, max(a) + step[1L] * (1 - params$hjust))
    v <- c(min(b) - step[2L] * params$vjust, max(b) + step[2L] * (1 - params$vjust))
    texture_quad_packet_3d(texture_3d(image, interpolate = isTRUE(params$interpolate)), u, v,
      data[[axes[3L]]][rows[1L]], axes, data[rows, , drop = FALSE], params, panel_params)
  })
  merge_scene_packets(packets, panel_params$panel_id)
}

register_geom_3d(GeomRaster3D, emit_raster_3d,
  parameters = list(axes = c("x", "y", "z"), at = NULL, interpolate = FALSE, hjust = 0.5, vjust = 0.5))
