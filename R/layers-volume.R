GeomVolume3D <- ggplot2::ggproto("GeomVolume3D", ggplot2::Geom,
  required_aes = c("x", "y", "z", "value"), default_aes = ggplot2::aes(),
  draw_key = ggplot2::draw_key_blank
)

volume_transfer_3d <- function(field, opacity, colours) {
  limits <- range(field$value)
  if (diff(limits) == 0) limits <- limits + c(-1, 1) * max(1, abs(limits[1L])) * 1e-6
  knots <- seq(limits[1L], limits[2L], length.out = 256L)
  ramp <- if (is.function(opacity)) opacity(knots) else {
    if (!is.numeric(opacity) || !length(opacity)) stop("opacity must be a numeric ramp or function.", call. = FALSE)
    if (length(opacity) == 1L) rep(opacity, length(knots)) else
      stats::approx(seq(limits[1L], limits[2L], length.out = length(opacity)), opacity, knots)$y
  }
  if (!is.numeric(ramp) || !(length(ramp) %in% c(1L, length(knots))) || any(!is.finite(ramp) | ramp < 0))
    stop("opacity must give nonnegative finite extinction coefficients.", call. = FALSE)
  rgb <- if (is.function(colours)) colour_channels_3d(colours(knots)) / 255 else {
    if (!is.character(colours) || !length(colours) || anyNA(colours)) stop("colours must be a colour ramp or function.", call. = FALSE)
    t(grDevices::colorRamp(colours)(seq(0, 1, length.out = length(knots)))) / 255
  }
  if (ncol(rgb) == 1L) rgb <- rgb[, rep(1L, length(knots)), drop = FALSE]
  if (!identical(dim(rgb), c(3L, length(knots))) || any(!is.finite(rgb))) stop("colours must give one colour per transfer value.", call. = FALSE)
  list(knots = knots, sigma = rep_len(as.double(ramp), length(knots)), colours = t(srgb_decode_3d(rgb)))
}

emit_volume_3d <- function(data, params, panel_params) {
  if (any(!is.finite(as.matrix(data[c("x", "y", "z", "value")]))))
    stop("Volume coordinates and values must be finite; the complete grid is required.", call. = FALSE)
  for (axis in c("x", "y", "z")) data[[axis]] <- data[[axis]] * diff(panel_params$ranges[[axis]]) + panel_params$ranges[[axis]][1L]
  field <- stats_scalar_grid_3d(data)
  spec <- c(list(kind = "volume", mode = match(params$mode, c("absorption", "mip", "iso")) - 1L,
    samples = params$samples, level = params$level %or3d% mean(range(field$value))), field,
    volume_transfer_3d(field, params$opacity, params$colours))
  if (length(spec$level) != 1L || !is.finite(spec$level)) stop("level must be one finite number.", call. = FALSE)
  raymarch_payload_3d(spec, params, panel_params)
}

register_geom_3d(GeomVolume3D, emit_volume_3d, aesthetics = "value",
  parameters = list(mode = "absorption", opacity = c(0, 1), colours = c("#132B43", "#56B1F7"), samples = 256L, level = NULL))

#' Ray-marched scalar volumes
#'
#' Renders a complete rectilinear grid in the raster backend. Coordinates are
#' interpreted in transformed scale units. Samples are trilinearly interpolated.
#' Absorption uses front-to-back emission-absorption integration in linear light
#' and stops at the nearest opaque mesh or traced surface. Overlapping volumes
#' combine their extinction and emission at each sample.
#'
#' MIP finds the maximum of the interpolated field along each ray. Isosurfaces
#' find the first root of the cubic field along each crossed grid cell, including
#' roots between equal-sign cell endpoints, and use its analytic gradient.
#' These modes are independent of the absorption sampling count.
#'
#' @param mapping,data,...,show.legend,inherit.aes Standard layer arguments.
#'   Map `x`, `y`, `z` and `value` to the grid coordinates and scalar field.
#' @param mode `"absorption"`, `"mip"` or `"iso"`.
#' @param opacity Nonnegative extinction coefficients per transformed data unit,
#'   as an evenly spaced numeric ramp over the scalar range, or a function of
#'   scalar values. A constant coefficient s over distance d transmits exp(-s*d).
#' @param colours Colour ramp, or a function mapping scalar values to colours.
#' @param samples Number of integration steps across the volume diagonal.
#'   Grid-cell boundaries also split integration intervals.
#' @param level Isovalue for `mode = "iso"`; defaults to the scalar-range midpoint.
#' @return A ggplot2 layer.
#' @references Max (1995). Optical models for direct volume rendering.
#'   IEEE Transactions on Visualization and Computer Graphics 1, 99-108.
#'   \doi{10.1109/2945.468400}
#' @export
#' @examples
#' a <- seq(-1, 1, length.out = 8)
#' d <- expand.grid(x = a, y = a, z = a)
#' d$v <- with(d, exp(-4 * (x^2 + y^2 + z^2)))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = v)) +
#'   geom_volume_3d(opacity = c(0, 3), samples = 32) +
#'   coord_3d(backend = backend_3d("raster", dpi = 60))
geom_volume_3d <- function(mapping = NULL, data = NULL, ..., mode = c("absorption", "mip", "iso"),
                           opacity = c(0, 1), colours = c("#132B43", "#56B1F7"), samples = 256L,
                           level = NULL, show.legend = NA, inherit.aes = TRUE) {
  if (!is.numeric(samples) || length(samples) != 1L || !is.finite(samples) || samples < 1 ||
      samples != trunc(samples) || samples > .Machine$integer.max) stop("samples must be a positive integer.", call. = FALSE)
  make_layer_3d(GeomVolume3D, mapping, data, "identity", "identity",
    stats_params_3d(list(mode = match.arg(mode), opacity = opacity, colours = colours,
      samples = as.integer(samples), level = level), list(...)), show.legend, inherit.aes)
}
